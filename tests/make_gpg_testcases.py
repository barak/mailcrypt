#! /usr/bin/python

import os, commands, re, time
import GnuPGInterface

# emit test case files into test-cases/*

# keys:
#  1: owner1 <user@test>
#  2: owner2 <user@test>
#  3: other <other@test>
#  4: unknown key

homedir = "gpg-keys/full-rings"
testcasedir = "gpg-testcases"
users = ("owner1", "owner2", "other", "unknown", "trusted", "untrusted")

def get_keyid(name):
    # now what keyid did it get?
    cmd = "gpg --homedir %s --with-colons --fingerprint %s" % (homedir,name)
    (s,out) = commands.getstatusoutput(cmd)
    assert(s==0)
    r = re.search(r'^fpr:::::::::(\w+):',out, re.M)
    if r == None:
        print "problem, out '%s'" % out
        return None
    id = r.group(1) # long form
    return id

class Encrypt(GnuPGInterface.GnuPG):
    def __init__(self):
        GnuPGInterface.GnuPG.__init__(self)
        self.setup_my_options()

    def setup_my_options(self):
        self.options.armor = 1
        self.options.meta_interactive = 0
        self.options.extra_args.append('--no-secmem-warning')
        self.options.homedir = homedir

    def encrypt_string(self, string, recipients):
        self.options.recipients = recipients   # a list!
        proc = self.run(['--encrypt'], create_fhs=['stdin', 'stdout'])
        proc.handles['stdin'].write(string)
        proc.handles['stdin'].close()
        output = proc.handles['stdout'].read()
        proc.handles['stdout'].close()
        proc.wait()
        return output

    def signencrypt_string(self, string, signer, passphrase, recipients):
        self.options.recipients = recipients   # a list!
        self.options.default_key = signer
        self.passphrase = passphrase
        proc = self.run(['--sign', '--encrypt'],
                        create_fhs=['stdin', 'stdout'])
        proc.handles['stdin'].write(string)
        proc.handles['stdin'].close()
        output = proc.handles['stdout'].read()
        proc.handles['stdout'].close()
        proc.wait()
        return output

    def sign_string(self, string, signer, passphrase):
        self.options.recipients = []
        self.options.default_key = signer
        self.passphrase = passphrase
        proc = self.run(['--sign'],
                        create_fhs=['stdin', 'stdout'])
        proc.handles['stdin'].write(string)
        proc.handles['stdin'].close()
        output = proc.handles['stdout'].read()
        proc.handles['stdout'].close()
        proc.wait()
        return output

    def sym_string(self, string, passphrase):
        self.options.recipients = []
        self.passphrase = passphrase
        proc = self.run(['--symmetric'],
                        create_fhs=['stdin', 'stdout'])
        proc.handles['stdin'].write(string)
        proc.handles['stdin'].close()
        output = proc.handles['stdout'].read()
        proc.handles['stdout'].close()
        proc.wait()
        return output

    def armor_string(self, string):
        self.options.recipients = []
        proc = self.run(['--enarmor'],
                        create_fhs=['stdin', 'stdout'])
        proc.handles['stdin'].write(string)
        proc.handles['stdin'].close()
        output = proc.handles['stdout'].read()
        proc.handles['stdout'].close()
        proc.wait()
        return output

    def clearsign_string(self, string, signer, passphrase):
        self.options.recipients = []
        self.options.default_key = signer
        self.passphrase = passphrase
        proc = self.run(['--clearsign'],
                        create_fhs=['stdin', 'stdout'])
        proc.handles['stdin'].write(string)
        proc.handles['stdin'].close()
        output = proc.handles['stdout'].read()
        proc.handles['stdout'].close()
        proc.wait()
        return output

def emit_alist(f, d):
    f.write("(\n")
    keys = d.keys()
    keys.sort()
    for k in keys:
        if d[k] == None:
            f.write("(%s . nil)\n" % (k))
        else:
            f.write("(%s . \"%s\")\n" % (k, d[k]))
    f.write(")\n")

encryptor = Encrypt()
def encrypt(to, plaintext):
    return encryptor.encrypt_string(plaintext, [to])
def signencrypt(to, signer, plaintext):
    return encryptor.signencrypt_string(plaintext, signer, signer, [to])
def sign(signer, plaintext):
    return encryptor.sign_string(plaintext, signer, signer)
def sym(passphrase, plaintext):
    return encryptor.sym_string(plaintext, passphrase)
def armor(plaintext):
    # note: this isn't very useful, you must use 'gpg --dearmor' to
    # extract the result, not --decrypt. It also has a different banner.
    return encryptor.armor_string(plaintext)
def clearsign(signer, plaintext):
    return encryptor.clearsign_string(plaintext, signer, signer)

#def encrypt(to, plaintext):
    #cmd = "gpg --armor --homedir %s --recipient %s --batch --encrypt" % (homedir, to)
    #(stdout,stdin) = popen2.popen2(cmd)
    #stdin.write(plaintext)
    #stdin.close()
    #crypttext = stdout.read()
    #return crypttext

def make_plaintext():
    return "This is a plaintext message\n"
def date_string():
    # mc-gpg.el takes the YYYY-MM-DD date string from the SIG_ID status-fs
    # line and delivers it to the user. Dunno if this is GMT or localtime,
    # need to check in the evening.
    # return time.strftime("%Y-%m-%d", time.gmtime()) # GMT
    return time.strftime("%Y-%m-%d", time.localtime()) # localtime


def e(filename, recip):
    plaintext = make_plaintext()
    d = {}
    d['name'] = filename
    d['encryption_id'] = "%s <%s@test>" % (recip, recip)
    d['passphrase'] = recip
    if recip == "other" or recip == "unknown":
        d['error'] = "This message is not addressed to you"
        d['plaintext'] = None
    else:
        d['error'] = None
        d['plaintext'] = plaintext
    d['signature_status'] = None
    d['crypttext'] = "\n" + encrypt(recip, plaintext)
    f = open(os.path.join(testcasedir,filename), "w")
    emit_alist(f, d)
    f.close()

# get keyids
id = {}
for u in users:
    id[u] = get_keyid(u)

trustmap = {
    'owner1': "ULTIMATE",
    'owner2': "ULTIMATE",
    'other': "FULL",
    'unknown': "NONE",
    'trusted': "MARGINAL",
    'untrusted': "UNDEFINED",
    }

def es(filename, recip, signer):
    plaintext = make_plaintext()
    d = {}
    d['name'] = filename
    d['encryption_id'] = "%s <%s@test>" % (recip, recip)
    d['passphrase'] = recip
    d['signature_status'] = ("Good signature from '%s <%s@test>' " + \
                             "TRUST_%s made %s") % \
                             (signer, signer,
                              trustmap[signer], date_string())
    if signer == "unknown":
        d['signature_status'] = "cannot check signature from keyid %s" % \
                                id[signer][-16:]
        # comes from 'cannot check signature' warning
    if recip == "other" or recip == "unknown":
        d['error'] = "This message is not addressed to you"
        d['plaintext'] = None
        d['signature_status'] = None
    else:
        d['error'] = None
        d['plaintext'] = plaintext
    d['crypttext'] = "\n" + signencrypt(recip, signer, plaintext)
    f = open(os.path.join(testcasedir,filename), "w")
    emit_alist(f, d)
    f.close()

def s(filename, signer):
    plaintext = make_plaintext()
    d = {}
    d['name'] = filename
    d['error'] = None
    d['plaintext'] = plaintext
    d['signature_status'] = ("Good signature from '%s <%s@test>' " + \
                             "TRUST_%s made %s") % \
                             (signer, signer,
                              trustmap[signer], date_string())
    if signer == "unknown":
        d['signature_status'] = "cannot check signature from keyid %s" % \
                                id[signer][-16:]
        # comes from 'cannot check signature' warning
    d['crypttext'] = "\n" + sign(signer, plaintext)
    f = open(os.path.join(testcasedir,filename), "w")
    emit_alist(f, d)
    f.close()

def cs(filename, signer):
    plaintext = make_plaintext()
    d = {}
    d['name'] = filename
    d['error'] = None
    d['plaintext'] = plaintext
    d['signature_status'] = ("Good signature from '%s <%s@test>' " + \
                             "TRUST_%s made %s") % \
                             (signer, signer,
                              trustmap[signer], date_string())
    if signer == "unknown":
        d['signature_status'] = "cannot check signature from keyid %s" % \
                                id[signer][-16:]
        # comes from 'cannot check signature' warning
    d['crypttext'] = "\n" + clearsign(signer, plaintext)
    f = open(os.path.join(testcasedir,filename), "w")
    emit_alist(f, d)
    f.close()

def make_cases():
    e("E.e1r", "owner1")
    e("E.e2r", "owner2")
    e("E.e3", "other")
    e("E.e4", "unknown")
    es("ES.e1r.s1v", "owner1", "owner1")
    es("ES.e1r.s2v", "owner1", "trusted")
    es("ES.e1r.s3v", "owner1", "untrusted")
    es("ES.e1r.s4", "owner1", "unknown")
    es("ES.e3.s1v", "other", "owner1")
    es("ES.e4.s1v", "unknown", "owner1")
    s("S.s1v", "owner1")
    s("S.s2v", "trusted")
    s("S.s3v", "untrusted")
    s("S.s4", "unknown")
    sym("SE", "password")
    cs("CS.s1v", "owner1")
    cs("CS.s2v", "trusted")
    cs("CS.s3v", "untrusted")
    cs("CS.s4", "unknown")
    
if __name__ == "__main__":
    os.mkdir(testcasedir)
    make_cases()
    

# todo:
#  hardcoded date in es()
#  cs(): corrupt the signatures by editing the crypttext after signing
