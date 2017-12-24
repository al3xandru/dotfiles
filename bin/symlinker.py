#!/usr/bin/env python
import os
import sys

def scan_directory(dirname):
    """ Scans the given directory for all symlinks and aliases and
    returns a list of tuples
    """
    results = []
    if os.path.islink(dirname):
        results.append(('.', user_relative_path(os.path.abspath(os.readlink(dirname)))))
    abs_dirname = os.path.abspath(dirname)
    for root, dirs, files in os.walk(dirname):
        for f in dirs:
            fullpath = os.path.join(root, f)
            if os.path.islink(fullpath):
                target = os.readlink(fullpath)
                if os.path.isabs(os.path.expanduser(target)):
                    # symlink is external to the project
                    results.append((False, relative_path(fullpath, dirname), user_relative_path(os.path.abspath(target))))
                else:
                    # symlink is in the same project
                    results.append((False, relative_path(fullpath, dirname), relative_path(os.path.join(os.path.dirname(fullpath), target), dirname)))

        for f in files:
            fullpath = os.path.join(root, f)
            if os.path.islink(fullpath):
                target = os.readlink(fullpath)
                if os.path.isabs(os.path.expanduser(target)):
                    # symlink is external to the project
                    results.append((False, relative_path(fullpath, dirname), user_relative_path(os.path.abspath(target))))
                else:
                    # symlink is in the same project
                    results.append((True, relative_path(fullpath, dirname), target))
    return results

def relative_path(fullpath, rootpath):
    """ Calculates the relative path of `fullpath` versus `rootpath` """
    if not rootpath.endswith('/'):
        rootpath += '/'
    return "./" + fullpath[len(rootpath):]

def user_relative_path(fullpath):
    """ Calculates the relative path of `fullpath` versus the current user home directory.
    If `fullpath` is not under user's home directory it returns the full path"""
    user_home_path = os.path.expanduser("~")
    if fullpath.startswith(user_home_path):
        return "~" + fullpath[len(user_home_path):]
    else:
        return fullpath

def scan_file(filename):
    """ Checks if the `filename` is a symlink/alias. If `true` it returns
    a tuple with the 2nd value being the target. If `false` return None
    """
    if os.path.islink(filename):
        return filename, user_relative_path(os.path.abspath(os.readlink(fullpath)))
    return None

def restore_symlinks(basedir, conf_file):
    confs = read_configuration(conf_file)
    if not confs:
        print 'No symlinks defined'
        return
    if confs[0][1] == '.':
        print "WARNING: The root directory is a symlink to %s" % confs[0][2]
    results = []
    for rel, l, t in confs:
        if l != '.':
            absl = os.path.join(basedir, l[2:])
            if os.path.exists(absl):
                if not os.path.islink(absl):
                    if os.path.isdir(absl):
                        results.append("rm -rvf %s" % absl)
                        if rel:
                            results.append("cd %s" % os.path.dirname(l))
                            results.append("ln -s %s %s" % (t, os.path.basename(l)))
                            results.append("cd %s" % basedir)
                        else:
                            results.append("ln -s %s %s" % (t, l))
                    else:
                        results.append("rm -vf %s" % absl)
                        if rel:
                            results.append("cd %s" % os.path.dirname(l))
                            results.append("ln -s %s %s" % (t, os.path.basename(l)))
                            results.append("cd %s" % basedir)
                        else:
                            results.append("ln -s %s %s" % (t, l))
                else:
                    tlink = os.readlink(absl)
                    if tlink != t:
                        results.append("%s points to %s instead of %s" % (l, tlink, t))
            else:
                if rel:
                    results.append("cd %s" % os.path.dirname(l))
                    results.append("ln -s %s %s" % (t, os.path.basename(l)))
                    results.append("cd %s" % basedir)
                else:
                    results.append("ln -s %s %s" % (t, l))
    return results



def read_configuration(conf_file):
    f = open(conf_file, 'r')
    confs = []
    for l in f.readlines():
        parts = l.split('->')
        if parts[0].strip().startswith('!R '):
            confs.append((True, parts[0][3:], parts[1].strip()))
        else:
            confs.append((False, parts[0].strip(), parts[1].strip()))
    f.close()
    return confs

if __name__ == '__main__':
    if len(sys.argv) == 1:
        print "Scanning current directory:", os.getcwd()
        result = scan_directory(os.getcwd())
        for rel, l, t in result:
            if rel:
                print '!R', l, '->', t
            else:
                print l, '->', t
    else:
        input = sys.argv[1]
        print input
        if not os.path.exists(input):
            print >> sys.stderr, 'Input must be a valid file or directory'
            sys.exit(1)
        if os.path.isdir(input):
            print >> sys.stderr, 'symlinker.py must be run from the directory to be scanned'
            sys.exit(3)
        else:
            input = os.path.abspath(input)
            name, ext = os.path.splitext(input)
            if ext and ext == ".symls":
                if len(sys.argv) > 2:
                    basedir = sys.argv[2]
                else:
                    basedir = os.path.dirname(input)
                if not os.path.exists(basedir) or not os.path.isdir(basedir):
                    print >> sys.stderr, "Basedir %s is not a directory" % basedir
                    sys.exit(2)
                current_dir = os.getcwd()
                os.chdir(basedir)
                results = restore_symlinks(basedir, input)
                if results:
                    print "Run the following commands to restore symlinks"
                    print
                    if current_dir != basedir:
                        print "cd %s" % basedir
                    for cmd in results:
                        print cmd
            else:
                result = scan_file(input)
                if result:
                    print result[0], '->', result[1]

# For Mac OS Aliases:
#
# AppleScript:
#
# tell application "Finder"
#	set theFileType to get kind of ((POSIX file "/Users/alex/workspace/python/scripts/00-inbox") as alias)
#	display dialog "Type:" & theFileType buttons {"Yes", "No"} default button 2 with icon 1
# end tell
#
# Bash:
#
# mdls utility
#
# Reading list:
#
# http://www.cocoatech.com/weblog/archives/2007/04/30/aliases_symboli
# http://hints.macworld.com/article.php?story=2001110610290643
# http://hints.macworld.com/article.php?story=20061201041424401
# http://www.macworld.com/article/144680/2009/12/symboliclinker.html
# http://stdout.caiochassot.com/post/1241385588/script-finder-paste-as-symlink
# http://incrementalism.net/tech/making-a-symbolic-link-or-a-new-text-file-from-the-finder
#
# Dropbox:
#
# http://forums.dropbox.com/topic.php?id=17579
# http://forums.dropbox.com/topic.php?id=15271
