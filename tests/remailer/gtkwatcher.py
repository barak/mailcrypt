#! /usr/bin/python

if __name__ == '__main__':
    import pygtk
    pygtk.require("2.0")

import time
import gobject, gtk, gtk.glade
from watcher import Watcher

class WatcherGUI:
    def __init__(self, watcher):
        self.watcher = watcher
        watcher.gui = self

        self.done = 0
        
        xml = gtk.glade.XML('gtkwatcher.glade')
        self.xml = xml
        xml.signal_connect('do_poll', self.do_poll)
        xml.signal_connect('do_exit', self.do_quit)
        self.src_model = gtk.ListStore(gobject.TYPE_STRING,
                                       gobject.TYPE_STRING,
                                       gobject.TYPE_PYOBJECT)
        self.dst_model = gtk.ListStore(gobject.TYPE_STRING,
                                       gobject.TYPE_STRING,
                                       gobject.TYPE_PYOBJECT)
        
        view = xml.get_widget('src_treeview')
        view.set_model(self.src_model)
        r = gtk.CellRendererText()
        view.append_column(gtk.TreeViewColumn("Message ID", r, text=0))
        view.append_column(gtk.TreeViewColumn("Message Sent", r, text=1))
        sel = view.get_selection()
        sel.set_mode(gtk.SELECTION_SINGLE)
        sel.connect("changed", self.do_src_select)
        self.src_sel = sel
        
        view = xml.get_widget('dst_treeview')
        view.set_model(self.dst_model)
        r = gtk.CellRendererText()
        view.append_column(gtk.TreeViewColumn("Message ID", r, text=0))
        view.append_column(gtk.TreeViewColumn("Latency", r, text=1))
        sel = view.get_selection()
        sel.set_mode(gtk.SELECTION_SINGLE)
        sel.connect("changed", self.do_dst_select)
        self.dst_sel = sel
        
        self.srcwin = xml.get_widget('srcwin')
        self.dstwin = xml.get_widget('dstwin')
        #self.src_clist.connect('select-row', self.do_src_select)
        #self.dst_clist.connect('select-row', self.do_dst_select)
        self.text = xml.get_widget('text1')
        self.text.set_wrap_mode(gtk.WRAP_NONE)
        self.textwin = xml.get_widget('textwin')
        xml.get_widget('window1').set_size_request(500,300)
        self.do_update()

    def do_quit(self, widget):
        print "doing quit"
        self.done = 1
        # mainquit asserts, because we aren't actually in a mainloop
        #gtk.mainquit()
        
    def time_string(self, latency):
        hours = latency / 3600
        latency -= hours * 3600
        minutes = latency / 60
        latency -= minutes * 60
        seconds = latency
        latency = ''
        if hours:
            latency = '%dh' % hours
        if hours or minutes:
            latency = latency + '%dm' % minutes
        latency = latency + '%ds' % seconds
        return latency
    def update_text(self, text):
        buf = self.text.get_buffer()
        buf.set_text(text)
        # now make the end of the buffer visible
        # XXX: this flashes. They removed freeze/thaw.. how to fix?
        iter = buf.get_iter_at_line(-1)
        #print iter.get_line()
        # turn it into a mark, as scroll_to_iter depends upon height
        # calculations that are done in an idle task, so it won't get it right
        # until later
        mark = buf.create_mark("end", iter, 0)
        self.text.scroll_to_mark(mark, within_margin=0)
        
    def do_src_select(self, sel):
        model, iter = sel.get_selected()
        if not iter:
            return  # deselected
        m = model.get_value(iter, 2)
        # get the message text from the Outstanding list
        text = m.data
        self.update_text(text)
        # need to deselect the one in the other list so we can sense when it
        # becomes reselected
        self.dst_sel.unselect_all()
        
    def do_dst_select(self, sel):
        model, iter = sel.get_selected()
        if not iter:
            return
        m = model.get_value(iter, 2)
        text = m.data
        self.update_text(text)
        self.src_sel.unselect_all()

    def do_poll(self, *args):
        self.watcher.poll()
        return 1
    def do_update(self):
        src = self.src_model
        src.clear()
        for msg, txtime in self.watcher.outstanding():
            #elapsed = self.time_string(int(time.time()) - txtime)
            sent = time.strftime("%H:%M   %d %b %Y",time.gmtime(txtime))
            iter = src.append()
            src.set(iter,
                    0, msg.msgid,
                    1, sent,
                    2, msg)
            #ol.set_row_data(row, msgid)
            #ol.set_selectable(row, 0)
        #ol.moveto(maxrow, 0, 1.0, 0.0)

        dst = self.dst_model
        dst.clear()
        for msg, latency in self.watcher.received():
            iter = dst.append()
            dst.set(iter,
                    0, msg.msgid,
                    1, self.time_string(latency),
                    2, msg)
            #rl.set_row_data(row, msgid)
            #rl.set_selectable(row, 0)
        #rl.moveto(maxrow, 0, 1.0, 0.0)

class GtkWatcher(Watcher):
    def checker(self, m):
        self.gui.do_update()
    def start(self):
        self.gui = WatcherGUI(self)
        Watcher.start(self)
    def stop(self):
        self.gui = None
        Watcher.stop(self)

        
def main():
    from gtkwatcher import GtkWatcher
    from watcher import DirWatcher
    w = GtkWatcher()
    sdir = DirWatcher("sdir")
    w.addSource(sdir)
    ddir = DirWatcher("ddir")
    w.addDest(ddir)
    print "running"
    w.start()
    while not w.gui.done:
        gtk.mainiteration()
    print "done"
    w.stop()
    
if __name__ == '__main__': main()
