#! /usr/bin/python

import os, os.path, stat

# This program watches a pair of maildir-style directories. A message is
# placed in the 'source' maildir when the user runs an elisp function just
# before encrypting the message for a remailer chain. The 'dest' maildir
# should be on the receiving end of a .qmail or .forward file. This program
# polls both directories every ten seconds, watching for changes. It creates
# a list of all files in 'source' that are not matched by files in 'dest'.

class DirWatcher:
    def __init__(self, d):
        self.dir = d
        self.files = {}
        self.mtime = 0
        self.make_maildir(d)
    def make_maildir(self, which):
        try:
            os.mkdir(which)
            os.mkdir(os.path.join(which, "new"))
            os.mkdir(os.path.join(which, "tmp"))
        except OSError:
            return
        except:
            raise
    def poll(self):
        newdir = os.path.join(self.dir, "new")
        dir_mtime = os.stat(newdir)[stat.ST_MTIME]
        if dir_mtime > self.mtime:
            self.mtime = dir_mtime
            self.files = {}
            for f in os.listdir(newdir):
                fd = open(os.path.join(newdir,f))
                self.files[f] = {}
                self.files[f]['msgid'] = None
                data = ''
                for line in fd.readlines():
                    data = data + line
                    key = "MailcryptRemailerMessageId="
                    where = line.find(key)
                    if where != -1:
                        where = where + len(key)
                        self.files[f]['msgid'] = line[where:-1]
                fd.close()
                self.files[f]['data'] = data
    def dump(self):
        print "dir:", self.dir
        print len(self.files)," files:"
        for f in self.files.keys():
            print f, "%d bytes, id %s" % \
                  (len(self.files[f]['data']), self.files[f]['msgid'])
        print
    def msgids(self):
        ids = []
        map(lambda x, self=self, ids=ids: ids.append(self.files[x]['msgid']),
            self.files.keys())
        return ids
                                               
class Watcher:
    def __init__(self, source, dest):
        self.source = DirWatcher(source)
        self.dest = DirWatcher(dest)
    def poll(self):
        self.source.poll()
        self.dest.poll()
    def incomplete(self):
        waiting = []
        s_ids = self.source.msgids()
        d_ids = self.dest.msgids()
        for i in s_ids:
            if i not in d_ids:
                waiting.append(i)
        return waiting

def test1():
    w = Watcher("sdir", "ddir")
    print "polling"
    import time
    while 1:
        time.sleep(5)
        w.poll()
        w.source.dump()
        w.dest.dump()
        print

def test2():
    w = Watcher("sdir", "ddir")
    print "polling"
    import time
    while 1:
        w.poll()
        waiting = w.incomplete()
        print waiting
        time.sleep(5)

if __name__ == '__main__':
    test2()
    
