#! /usr/bin/python

# This is a class which watches a maildir for new messages. It uses the
# linux dirwatcher API (if available) to look for new files. The
# .messageReceived method is invoked with the filename of the new message,
# relative to the top of the maildir (so it will look like "new/blahblah").

# This version is implemented as a Twisted Python "Service". It uses the
# twisted Reactor to handle polling and signal safety.

from twisted.internet.app import ApplicationService
from twisted.internet import reactor
from maildir import Maildir

class MaildirTwisted(ApplicationService, Maildir):
    def __init__(self, serviceName, serviceParent, basedir):
        ApplicationService.__init__(self, serviceName)
        Maildir.__init__(self, basedir)
    def startService(self):
        self.serviceRunning = 1
        self.start()
        return None
    def stopService(self):
        self.stop()
        return None
    def startTimeout(self):
        self.timeout = reactor.callLater(self.pollinterval, self.poll)
    def stopTimeout(self):
        if self.timeout:
            self.timeout.cancel()
            self.timeout = None
    def dnotify_callback(self):
        # make it safe
        reactor.callFromThread(self.poll)


def test1():
    class MaildirTest(MaildirTwisted):
        def messageReceived(self, filename):
            print "changed:", filename
    from twisted.internet.app import Application
    #from maildir import MaildirTest
    # note that MaildirTest is really __main__.MaildirTest, which makes
    # maildir-test-shutdown.tap unusable
    app = Application("maildir-test")
    # add the service to the app
    m = MaildirTest("maildir", app, basedir="ddir")
    print "watching ddir/new/"
    app.run()
    print "done"
    
if __name__ == '__main__':
    test1()
    

