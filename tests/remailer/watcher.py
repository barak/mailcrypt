#! /usr/bin/python

if __name__ == '__main__':
    import pygtk
    pygtk.require("2.0")

import gtk

import os, os.path, stat, time, string
from maildirgtk import MaildirGtk
from nntplib import NNTP, NNTP_PORT, NNTPError

# This file implements some classes which watch maildir-style directories
# and NNTP newsgroups. A message is placed in the 'source' maildir when the
# user runs an elisp function just before encrypting the message for a
# remailer chain. Messages that emerge from the chain as email will wind up
# in the 'dest' maildir, which should be on the receiving end of a .qmail or
# .forward file. Messages that emerge as news postings should appear in a
# newsgroup monitored by the NewsWatcherService.

# The Watcher class watches both the source directory and the target
# dir/groups. It can provide a list of all files in 'source' that are not
# matched by files in 'dest'.

class Message:
    def __init__(self, msgid, name, time, data):
        self.msgid = msgid # the number inserted by the elisp counter
        self.name = name
        self.time = time # when the message was sent / arrived
        self.data = data # contents of the mesage (inc headers)
    def __repr__(self):
        return "<Message #%s>" % self.msgid
    
class MessageWatcher:
    def __init__(self):
        self.msgs = {}
        self.checker = None

    def setChecker(self, checker):
        self.checker = checker
    def __getstate__(self):
        d = self.__dict__.copy()
        d['checker'] = None
        return d

    def parseMessage(self, filename, name, time, lines):
        # find messageid, build bodytext
        data = ''
        msgid = None
        key = "MailcryptRemailerMessageId="
        for line in lines:
            line = line.rstrip("\n")
            data = data + line + "\n"
            where = line.find(key)
            if where != -1:
                where = where + len(key)
                msgid = line[where:]
        if msgid == None:
            return
        msgid = int(msgid)
        m = Message(msgid, name, time, data)
        if self.msgs.get(msgid, None) != None:
            print "Hey, file %s duplicates msgid %s from file %s" % \
                  (filename, msgid, self.msgs[msgid].filename)
        self.msgs[msgid] = m
        if self.checker:
            self.checker(m)

    def deleteMessage(self, msgid):
        try:
            del self.msgs[msgid]
        except KeyError:
            pass
        
    def dump(self):
        print len(self.msgs)," msgids:"
        for m in self.msgs.values():
            print m.msgid, "%d bytes, file %s" % (len(m.data), m.name)
        print
        
    
class DirWatcher(MaildirGtk, MessageWatcher):
    """This object watches a single maildir for incoming messages. It scans
    them for the MailcryptRemailerMessageId number, and stashes them in the
    .msgids dictionary by id. It also gives them to a .checker function, if
    set. The Maildir class tracks files that have been seen already, and
    keeps a persistent list of them, so the checker will only be called once
    per message."""
    def __init__(self, basedir):
        MessageWatcher.__init__(self)
        MaildirGtk.__init__(self, basedir)

    # we inherit start() and stop() from Maildir via MaildirGtk
    
    def messageReceived(self, name):
        # name is relative to the directory being watched
        #print "messageReceived", self.basedir, name
        filename = os.path.join(self.basedir, name)
        mtime = os.stat(filename)[stat.ST_MTIME]
        fd = open(filename)
        self.parseMessage(filename, name, mtime, fd.readlines())
        fd.close()
        
    def dump(self):
        print "dir:", self.basedir
        MessageWatcher.dump(self)
        
class NewsWatcher(MessageWatcher):
    def __init__(self, server, groups,
                 user=None, pw=None, port=None, tag=None):
        MessageWatcher.__init__(self)
        self.server = server
        self.groups = groups
        self.nntp = None  # the NNTP connection object
        self.user = user
        self.pw = pw
        self.port = port
        self.tag = tag
        self.last = {}
        self.timeout = None
        self.pollInterval = 60
        self.debug = 0
        
    def __repr__(self):
        return "<NewsWatcher %s:%s (%s)>" % (self.server, self.port,
                                             ",".join(self.groups))
    def __getstate__(self):
        d = MessageWatcher.__getstate__(self)
        d['nntp'] = None # just in case
        return d
    
    def start(self):
        port = self.port
        if not port:
            port = NNTP_PORT
        self.nntp = NNTP(self.server, port, self.user, self.pw,
                         readermode=1)
        # only look for messages that appear after we start. Usenet is big.
        if not self.last: # only do this the first time
            for g in self.groups:
                resp, count, first, last, name = self.nntp.group(g)
                self.last[g] = int(last)
                if self.debug: print "last[%s]: %d" % (g, self.last[g])
        self.timeout = gtk.timeout_add(self.pollInterval*1000,
                                       self.doTimeout)
    
    def stop(self):
        self.nntp.quit()
        self.nntp = None
        if self.timeout:
            gtk.timeout_remove(self.timeout)
            self.timeout = None

    def doTimeout(self):
        self.poll()
        return gtk.TRUE # keep going
        
    def poll(self):
        #print "polling", self
        for g in self.groups:
            resp, count, first, last, name = self.nntp.group(g)
            for num in range(self.last[g]+1, int(last)+1):
                try:
                    resp, num, id, lines = self.nntp.article("%d" % num)
                except NNTPError:
                    continue
                name = "%s:%d" % (g, int(num))
                if self.debug: print "got", name
                if self.tag:
                    if not filter(lambda line, tag=tag: line.find(tag) != -1,
                                  lines):
                        continue
                self.parseMessage(name, name, time.time(), lines)
            self.last[g] = int(last)
        
class Watcher:
    def __init__(self):
        self.source = None
        self.dests = []
        self.watchers = []
    def checker(self, m):
        # something changed, somewhere. The message 'm' was added, but we don't
        # know where. Override this method.
        pass
        
    def addSource(self, source):
        self.source = source
        self.watchers.append(source)
    def addDest(self, dest):
        self.dests.append(dest)
        self.watchers.append(dest)
    def start(self):
        if self.source:
            self.source.setChecker(self.checker)
        for d in self.dests:
            d.setChecker(self.checker)
        for w in self.watchers:
            w.start()
    def stop(self):
        for w in self.watchers:
            w.stop()
            
    def poll(self):
        # force a poll
        self.source.poll()
        for d in self.dests:
            d.poll()
            
    def msgs(self):
        # return dict of sent messages, and list of received messages. msgid
        # is the key for both, and the Message is the value.
        src = self.source.msgs
        dst = {}
        for d in self.dests:
            dst.update(d.msgs)
        return (src, dst)
    def outstanding(self):
        """Return a list of tuples (srcmsg, time), where time is the absolute
        time the message was sent."""
        src, dst = self.msgs()
        outstanding = []
        for msgid in src.keys():
            if not dst.has_key(msgid):
                m = src[msgid]
                outstanding.append((m, m.time))
        outstanding.sort(lambda x,y: cmp(x[0].msgid, y[0].msgid))
        return outstanding
    def age(self, msgid):
        tx_time = self.txtime(msgid)
        if not tx_time:
            return None
        age = time.time() - tx_time
        return age
    def txtime(self, msgid):
        if not self.source.msgs.has_key(msgid):
            return None
        tx_time = self.source.msgs[msgid].time
        return tx_time
        
    def received(self):
        """Return a list of tuples (dstmsg, latency), where latency is in
        seconds."""
        received = []
        src, dst = self.msgs()
        for msgid in src.keys():
            if dst.has_key(msgid):
                txtime = src[msgid].time
                rxtime = dst[msgid].time
                received.append((dst[msgid], rxtime - txtime))
        received.sort(lambda x,y: cmp(x[0].msgid, y[0].msgid))
        return received
    def abandon(self, msgid):
        # give up on the message: remove it from the source list
        self.source.deleteMessage(msgid)
        # just in case, remove it from the dest lists too
        for d in self.dests:
            d.deleteMessage(msgid)
    def flush(self, msgid):
        return self.abandon(msgid)
    
    
def do_test(wclass):
    # watch two maildirs
    w = wclass()
    sdir = DirWatcher("sdir")
    w.addSource(sdir)
    ddir = DirWatcher("ddir")
    w.addDest(ddir)
    print "running"
    w.start()
    while 1:
        gtk.mainiteration()
    w.stop()
    print "finished"

def test1():
    class Test1Watcher(Watcher):
        def checker(self, m):
            self.source.dump()
            self.dests[0].dump()
            print
    do_test(Test1Watcher)
def test2():
    class Test2Watcher(Watcher):
        def checker(self, m):
            print "outstanding:", self.outstanding()
            print "complete:", self.received()
            print
    do_test(Test2Watcher)

if __name__ == '__main__':
    test2()
    

# TODO: make sure messages that are present at startup get counted too. For
# Maildir, this means doing the scan in start instead of __init__. Must
# decide about NewsWatcher, should probably do a scan at __init__ time,
# record last[] info, then drop connection and reestablish at start.

