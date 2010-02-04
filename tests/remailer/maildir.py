#! /usr/bin/python

# This is a class which watches a maildir for new messages. It uses the
# linux dirwatcher API (if available) to look for new files. The
# .messageReceived method is invoked with the filename of the new message,
# relative to the top of the maildir (so it will look like "new/blahblah").

# this is an abstract base class. It must be subclassed by something to
# provide a delay function (which polls in the case that DNotify isn't
# available) and a way to safely schedule code to run after a signal handler
# has fired. See maildirgtk.py and maildirtwisted.py for forms that use the
# event loops provided by Gtk+ and Twisted.

try:
    from dnotify import DNotify
    have_dnotify = 1
except:
    have_dnotify = 0
import os, os.path

class Maildir:
    def __init__(self, basedir):
        self.basedir = basedir
        self.newdir = os.path.join(basedir, "new")
        self.files = []
        self.pollinterval = 10  # only used if we don't have DNotify
        self.running = 0
        
    def start(self):
        """You must run start to receive any messages."""
        if self.running:
            return
        self.running = 1
        if not os.path.isdir(self.basedir) or not os.path.isdir(self.newdir):
            raise "invalid maildir '%s'" % basedir
        # we must hold an fd open on the directory, so we can get notified
        # when it changes.
        if have_dnotify:
            self.dnotify = DNotify(self.newdir, self.dnotify_callback,
                                   [DNotify.DN_CREATE])
        self.poll()

    def startTimeout(self):
        raise NotImplemented
    def stopTimeout(self):
        raise NotImplemented
    def dnotify_callback(self):
        print "callback"
        self.poll()
        raise NotImplemented
        
    def stop(self):
        if have_dnotify:
            self.dnotify.remove()
            del(self.dnotify)
        else:
            self.stopTimeout()
        self.running = 0

    def poll(self):
        # see what's new
        for f in self.files:
            if not os.path.isfile(os.path.join(self.newdir, f)):
                self.files.remove(f)
        newfiles = []
        for f in os.listdir(self.newdir):
            if not f in self.files:
                newfiles.append(f)
        self.files.extend(newfiles)
        for n in newfiles:
            self.messageReceived(os.path.join("new",n))
        if not have_dnotify:
            self.startTimeout()

    def messageReceived(self, filename):
        """Called when a new file is noticed. Override it in subclasses."""
        print filename


def test1():
    m = Maildir("ddir")
    m.start()
    import signal
    while 1:
        signal.pause()
    
if __name__ == '__main__':
    test1()
    
