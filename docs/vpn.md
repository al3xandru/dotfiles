# Cisco AnyConnect

Steps to use open source tools for connecting to a Cisco AnyConnect VPN, and open a local SOCKS5 proxy instead of directing all traffic. You do not have to give intrusive root permissions to any process, this whole setup runs completely in userland. This allows you to conditionally pass traffic to the VPN.

## Installation

### macOS

```shell
$ brew install openconnect
$ brew install ocproxy
```

### Docker

<details>
  <summary>Comming Soon (speak to me for possible beta testing)</summary>

```shell
$ docker pull kylef/ocproxy
$ alias openconnect="docker run -it -p 8912:8912 kylef/ocproxy openconnect"
```
</details>

### Other

- Install [openconnect](http://www.infradead.org/openconnect/packages.html)
- Install [ocproxy](https://github.com/cernekee/ocproxy#building-ocproxy)

## Usage

Connect to VPN and start SOCKS 5 Proxy on port `8912`:

```
$ openconnect --script-tun --script "ocproxy -D 8912" https://myaccess.oraclevpn.com
```

Enter Oracle VPN username and password when requested. `--user` may be provided to openconnect.

You now have a SOCKS5 proxy listening on `localhost:8912` which forwards traffic over the Oracle VPN.

**NOTE:** *If your system is configured to another language, you can switch openconnect to English using `LC_ALL=en_US` environment. Their translations are poor.*

### Automatic Traffic Routing

#### Browser Extensions

- [Proxy SwitchyOmega for Chrome](https://chrome.google.com/webstore/detail/proxy-switchyomega/padekgcemlokbadohgkifijomclgjgif)
- [Proxy SwitchyOmega for Firefox](https://addons.mozilla.org/en-US/firefox/addon/switchyomega/)
- Other SOCKS5 extensions may work.

<details>
  <summary>Configuration for SwitchyOmega</summary>

- Open Proxy SwitchyOmega Options
- New Profile
    - Name: `Oracle`
    - Type: `Proxy Profile`
    - Edit and Add Proxy Server:
        - Protocol: `SOCKS5`
        - Server: `localhost`
        - Port: `8912`
- New Profile
    - Name: `Auto`
    - Type: `Switch Profile`
    - Edit and Configure:
        - Add Condition
            - Type: `Host wildcard`
            - Condition Details: `*.oracle.com`
            - Profile: `Oracle`
        - Add Condition
            - Type: Host wildcard
            - Condition Details: `*.oraclecorp.com`
            - Profile: `Oracle`
        - Add Condition
            - Type: `Host wildcard`
            - Condition Details: `*.oraclevpn.com`
            - Profile: `Oracle`
        - Add Condition
            - Type: `Host wildcard`
            - Condition Details: `*.oraclecloud.com`
            - Profile: `Oracle`
        - Default Condition
            - Profile: `Direct`
- Apply Changes
- Ensure that "Auto" is the default and current profile.
</details>

### Command Line Tools using VPN

You can use set the `ALL_PROXY` environment variable to configure respective terminal tools. 

#### For single process

```shell
$ env ALL_PROXY=socks5h://localhost:8912 curl https://my.oracle.com -i
Location: https://my.oracle.com/index.htm
```

#### For current session

For bourne-compatible shells:

```shell
$ export ALL_PROXY=socks5h://localhost:8912
```

For fish:

```shell
$ set -x ALL_PROXY 'socks5h://localhost:8912'
```

**NOTE**: *You can also set `ALL_PROXY` in your shell configuration to apply settings to all new shells if desired.*

#### `git`

Use proxy for current repo (http/https):

```
$ git config http.proxy 'socks5h://localhost:8912'
```

**NOTE**: *`--global` can be used to configure proxy globally for all git repos.*

### 1Password Integration

Providing you have the [`op`](https://support.1password.com/command-line-getting-started/) (1Password CLI) installed and signed in, along with the tool `jq` (`brew install jq`) you can use the following script to connect to the VPN:

**NOTE**: *The Oracle VPN credentials including URL are expected to be found in 1Password under the item name `Oracle VPN`.*

```bash
#!/usr/bin/env bash

set -e

ITEM_NAME="Oracle VPN"
SOCKS_PORT=8912

ITEM=$(op get item "$ITEM_NAME")
URL=$(echo "$ITEM" | jq -r '.overview.url')
USERNAME=$(echo "$ITEM" | jq -r '.details.fields[] | select(.designation=="username").value')
PASSWORD=$(echo "$ITEM" | jq -r '.details.fields[] | select(.designation=="password").value')

echo "$PASSWORD" | openconnect --user "$USERNAME" --passwd-on-stdin --script-tun --script "ocproxy -D $SOCKS_PORT" "$URL" --disable-ipv6
```
