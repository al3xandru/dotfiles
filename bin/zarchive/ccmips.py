#!/usr/bin/env python
from __future__ import print_function

import os
import subprocess
import sys

def main(cluster_name, ips):
    log("Setting IPs for cluster {} to {}", cluster_name, ips)
    nodes = get_cluster_nodes(cluster_name)
    if len(nodes) > len(ips):
        log("[WARN] Not enough IPs ({}) for nodes ({})", len(ips), len(nodes))
    for node, ip in zip(nodes, ips):
        conf_file = get_conf_file(cluster_name, node)
        log("updating conf file {} to IP {}", conf_file, ip)
        update_node(conf_file, ip)
        log("rpc_address: {} in conf file {}", ip, conf_file)

def update_node(conf_file, ip):
    lines = []
    for l in open(conf_file, 'rb').readlines():
        if l.startswith('rpc_address:'):
            lines.append("rpc_address: %s" % ip)
            lines.append(os.linesep)
        else:
            lines.append(l)

    with open(conf_file, 'wb+') as fout:
        fout.write(''.join(lines))
        fout.flush()

def get_cluster_path(cluster_name):
    return os.path.join(os.path.expanduser('~'), '.ccm', cluster_name)

def get_cluster_nodes(cluster_name):
    nodes = []
    cluster_path = get_cluster_path(cluster_name)
    for f in os.listdir(cluster_path):
        if f != 'repository' and os.path.isdir(os.path.join(cluster_path, f)):
            nodes.append(f)

    return sorted(nodes)

def get_conf_file(cluster_name, node):
    return os.path.join(get_cluster_path(cluster_name), node, 'conf', 'cassandra.yaml')

def log(msg, *args):
    print(msg.format(*args))

def get_ips():
    output = subprocess.check_output("ifconfig | grep 'inet addr:' | grep 'Bcast' | sed s/Bcast.*// | sed s/inet\ addr:// | sed s/\ *//", 
            shell=True)
    return ' '.join(output.splitlines(False))

if __name__ == '__main__':
    if len(sys.argv) < 3:
        log("Available IPs: {}", get_ips())
        log("[ERROR] Usage {} cluster_name ip1 ...", sys.argv[0])
        sys.exit(1)
    cluster_name = sys.argv[1]
    ips = sys.argv[2:]
    if not os.path.isdir(get_cluster_path(cluster_name)):
        log("[ERROR] '{}' is not a known ccm cluster", cluster_name)
        sys.exit(2)
    main(cluster_name, ips)
