#! /usr/bin/python

if __name__ == '__main__':
    import pygtk
    pygtk.require("2.0")

import time, cPickle
import gobject, gtk, gtk.glade
from watcher import Watcher

def time_string(latency):
    if latency == None:
        return "?"
    latency = int(latency)
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

class WatcherGUI:
    def __init__(self, watcher):
        self.watcher = watcher
        watcher.gui = self

        self.done = 0
        
        xml = gtk.glade.XML('gtkwatcher.glade')
        self.xml = xml

        # main menu
        xml.signal_connect('do_poll', self.do_poll)
        xml.signal_connect('do_exit', self.do_quit)

        # source panel
        self.src_popup = xml.get_widget("src_popup")
        xml.signal_connect('do_src_abandon', self.do_src_abandon)
        xml.get_widget("source_message_options1").set_sensitive(0)
        self.src_age_item = xml.get_widget("src_age_item")
        self.src_age_item.set_sensitive(0)
        self.src_abandon_item = xml.get_widget("src_abandon_item")

        # dest panel
        self.dst_popup = xml.get_widget("dst_popup")
        xml.get_widget("dest_message_options1").set_sensitive(0)
        self.dst_sent_item = xml.get_widget("dst_sent_item")
        self.dst_sent_item.set_sensitive(0)
        self.dst_original_item = xml.get_widget("dst_original_item")
        self.dst_flush_item = xml.get_widget("dst_flush_item")
        
        xml.signal_connect('do_dst_flush', self.do_dst_flush)
        xml.signal_connect('do_dst_original', self.do_dst_original)

        # panel contents
        self.src_model = gtk.ListStore(gobject.TYPE_STRING,
                                       gobject.TYPE_STRING,
                                       gobject.TYPE_PYOBJECT)
        self.dst_model = gtk.ListStore(gobject.TYPE_STRING,
                                       gobject.TYPE_STRING,
                                       gobject.TYPE_PYOBJECT)
        
        view = xml.get_widget('src_treeview')
        view.connect("button_press_event", self.do_src_popup)
        view.set_model(self.src_model)
        r = gtk.CellRendererText()
        view.append_column(gtk.TreeViewColumn("Message ID", r, text=0))
        view.append_column(gtk.TreeViewColumn("Message Sent", r, text=1))
        sel = view.get_selection()
        sel.set_mode(gtk.SELECTION_SINGLE)
        sel.connect("changed", self.do_src_select)
        self.src_sel = sel
        
        view = xml.get_widget('dst_treeview')
        view.connect("button_press_event", self.do_dst_popup)
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
        
    def update_text(self, text, skipHeaders=0):
        buf = self.text.get_buffer()
        buf.set_text(text)
        # now make the end of the buffer visible
        # XXX: this flashes. They removed freeze/thaw.. how to fix?
        # XXX: if skipHeaders, find the first blank line and put that at top
        iter = buf.get_iter_at_line(-1)
        #print iter.get_line()
        # turn it into a mark, as scroll_to_iter depends upon height
        # calculations that are done in an idle task, so it won't get it right
        # until later
        mark = buf.create_mark("end", iter, 0)
        if skipHeaders:
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

    def do_src_popup(self, view, event):
        if event.button != 3:
            return
        pathset = view.get_path_at_pos(event.x, event.y)
        if pathset:
            path, viewcol, cell_x, cell_y = pathset
            iter = self.src_model.get_iter(path)
            m = self.src_model.get_value(iter, 2)
            age = self.watcher.age(m.msgid)
            label = self.src_age_item.get_child()
            label.set_text("Age[%d]: %s" % (m.msgid, time_string(age)))
            label = self.src_abandon_item.get_child()
            label.set_text("Abandon Message [%d]" % m.msgid)
            self.src_abandon_item.set_sensitive(1)
        else:
            label = self.src_age_item.get_child()
            label.set_text("Age: --")
            label = self.src_abandon_item.get_child()
            label.set_text("Abandon Message")
            self.src_abandon_item.set_sensitive(0)
        self.src_popup.popup(None, None, None, event.button, event.time)

    def do_src_abandon(self, menuitem):
        # which message? find the selection
        model, iter = self.src_sel.get_selected()
        if not iter:
            print "abandon, no iter!"
            return
        m = model.get_value(iter, 2)
        print "abandon msgid", m.msgid
        self.watcher.abandon(m.msgid)
        self.do_update()

    def do_dst_popup(self, view, event):
        if event.button != 3:
            return
        pathset = view.get_path_at_pos(event.x, event.y)
        if pathset:
            path, viewcol, cell_x, cell_y = pathset
            iter = self.dst_model.get_iter(path)
            m = self.dst_model.get_value(iter, 2)
            txtime = self.watcher.txtime(m.msgid)
            sent = time.strftime("%H:%M   %d %b %Y", time.localtime(txtime))
            label = self.dst_sent_item.get_child()
            label.set_text("Sent[%d]: %s" % (m.msgid, sent))
            self.dst_original_item.set_sensitive(1)
            label = self.dst_original_item.get_child()
            label.set_text("See Original [%d]" % m.msgid)
            self.dst_flush_item.set_sensitive(1)
            label = self.dst_flush_item.get_child()
            label.set_text("Flush Message [%d]" % m.msgid)
        else:
            label = self.dst_sent_item.get_child()
            label.set_text("Sent: --")
            self.dst_original_item.set_sensitive(0)
            label = self.dst_original_item.get_child()
            label.set_text("See Original")
            self.dst_flush_item.set_sensitive(0)
            label = self.dst_flush_item.get_child()
            label.set_text("Flush Message")
        self.dst_popup.popup(None, None, None, event.button, event.time)

    def do_dst_flush(self, menuitem):
        # which message? find the selection
        model, iter = self.dst_sel.get_selected()
        if not iter:
            return
        m = model.get_value(iter, 2)
        print "flush msgid", m.msgid
        self.watcher.flush(m.msgid)
        self.do_update()
        
    def do_dst_original(self, menuitem):
        # which message? find the selection
        model, iter = self.dst_sel.get_selected()
        if not iter:
            return
        dst_msg = model.get_value(iter, 2)
        src_msg = self.watcher.source.msgs.get(dst_msg.msgid, None)
        if src_msg:
            text = src_msg.data
            self.update_text(text, skipHeaders=0)
        #self.dst_sel.unselect_all()
        
    def do_dst_select(self, sel):
        model, iter = sel.get_selected()
        if not iter:
            return
        m = model.get_value(iter, 2)
        text = m.data
        self.update_text(text, skipHeaders=1)
        self.src_sel.unselect_all()

    def do_poll(self, *args):
        self.watcher.poll()
        return 1
    def do_update(self):
        src = self.src_model
        src.clear()
        for msg, txtime in self.watcher.outstanding():
            #elapsed = time_string(int(time.time()) - txtime)
            sent = time.strftime("%H:%M   %d %b %Y", time.localtime(txtime))
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
                    1, time_string(latency),
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

def makeWatcher():
    from gtkwatcher import GtkWatcher
    from watcher import DirWatcher, NewsWatcher
    import sys, getopt
    w = GtkWatcher()
    usage = """
    gtkwatcher.py
      --source sdir
      [--dest ddir]...
      --nntp host:port:user:pass [--group a]..
    """
    try:
        opts, args = getopt.getopt(sys.argv[1:], '',
                                   ['source=', 'dest=',
                                    'nntp=', 'group=', 'tag='])
    except getopt.GetoptError:
        print usage
        sys.exit(-1)
    nntp = None
    groups = []
    tag = None
    for key, value in opts:
        if key == "--source":
            print "adding source maildir '%s'" % value
            w.addSource(DirWatcher(value))
        if key == "--dest":
            print "adding dest maildir '%s'" % value
            w.addDest(DirWatcher(value))
        if key == "--nntp":
            nntp = value
        if key == "--group":
            groups.append(value)
        if key == "--tag":
            tag = value
    if groups and not nntp:
        raise "Must have --nntp server specified to use --group"
    if tag and not nntp:
        raise "Must have --nntp set to use message filtering tag"
    if nntp and not groups:
        raise "Gave NNTP server but no groups to poll: what's the point?"
    if nntp:
        # parse server spec
        fields = nntp.split(':')
        fields.extend([None] * (4 - len(fields)))
        host, port, user, passwd = fields
        if port:
            port = int(port)
        nw = NewsWatcher(host, groups,
                         user, passwd, port, tag)
        w.addDest(nw)
    return w

def main():
    try:
        f = open("watcher.pickle", "r")
        w = cPickle.load(f)
        f.close()
    except:
        print "unable to load pickle"
        w = makeWatcher()
    print "running"
    w.start()
    while not w.gui.done:
        gtk.mainiteration()
    print "done"
    w.stop()
    # save pickle of seen messages
    f = open("watcher.pickle", "w")
    cPickle.dump(w, f, 1)
    f.close()
    
if __name__ == '__main__': main()
