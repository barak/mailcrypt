#! /usr/bin/python

import watcher
import gtk
import libglade
import time

class GtkWatcher:
    def __init__(self, watcher=None):
        xml = libglade.GladeXML('watcher.glade')
        xml.signal_connect('do_poll', self.poll)
        xml.signal_connect('do_quit', gtk.mainquit)
        self.src_clist = xml.get_widget('src_clist')
        self.dst_clist = xml.get_widget('dst_clist')
        xml.get_widget('window1').set_usize(300,300)
        self.xml = xml
        self.watcher = watcher
        self.outstanding = []
        self.received = []
    def set_watcher(self, watcher=None):
        self.watcher = watcher
    def time_string(self, latency):
        hours = latency / 3600
        latency -= hours * 3600
        minutes = latency / 60
        latency -= minutes * 60
        seconds = latency
        latency = ''
        if hours:
            latency = '%dh ' % hours
        if hours or minutes:
            latency = latency + '%02dm ' % minutes
        latency = latency + '%02ds' % seconds
        return latency

    def poll(self, *args):
        self.watcher.poll()
        outstanding = self.watcher.outstanding()
        if outstanding != self.outstanding:
            ol = self.src_clist
            ol.freeze()
            ol.clear()
            for i in outstanding:
                (msgid, txtime) = i
                #elapsed = self.time_string(int(time.time()) - txtime)
                sent = time.strftime("%H:%M   %d %b %Y",time.gmtime(txtime))
                ol.append((msgid, sent))
            ol.thaw()
        self.outstanding = outstanding

        received = self.watcher.received()
        if received != self.received:
            rl = self.dst_clist
            rl.freeze()
            rl.clear()
            for i in received:
                (msgid, latency) = i
                rl.append((msgid, self.time_string(latency)))
            rl.thaw()
        self.received = received
        return 1
    

def main():
    w = watcher.Watcher('sdir', 'ddir')
    g = GtkWatcher(w)
    g.poll()
    dir_poller = gtk.timeout_add(5*1000, g.poll)
    gtk.mainloop()

if __name__ == '__main__': main()
