# Steps to install on haskell.org

The machine `hoogle.haskell.org` is located at `104.130.162.85`.

## Installation

### As `root`

	add-apt-repository -y ppa:hvr/ghc
	apt-get update
	apt-get install ghc-7.8.4 cabal-install-1.18 happy-1.19.4 alex-3.1.3
	apt-get install git
	apt-get install zlib1g-dev

Then create and user `www` configured for SSH access.

Create a swap file using the instructions [originally from here](https://www.digitalocean.com/community/tutorials/how-to-add-swap-on-ubuntu-14-04):

	fallocate -l 4G /swapfile
	chmod 600 /swapfile
	mkswap /swapfile
	swapon /swapfile

Then add the `/swapfile none swap sw 0 0` line to the bottom of `/etc/fstab`.


### As `www`

	git clone https://github.com/ndmitchell/hogle.git

Then follow the updating steps.

## Updating

	pkill hogle
	cd hogle
	export PATH=/home/www/.cabal/bin:/opt/ghc/7.8.4/bin:/opt/cabal/1.18/bin:/opt/happy/1.19.4/bin:/opt/alex/3.1.3/bin:$PATH
	git pull
	cabal update
	cabal install --ghc-options=-rtsopts
	hogle generate +RTS -M1G
	hogle test
	nohup hogle server --port=8080 --log=log.txt &
	echo Started

These commands are also stored as `/home/www/update.sh`.

## Monitoring

* `df -h`, check there is sufficient disk space.
* `top`, see what is running.

Currently monitored with [uptimerobot.com](http://uptimerobot.com/).

## Enhancements

To run on port 80, as root do (as per [here](http://stackoverflow.com/questions/413807/is-there-a-way-for-non-root-processes-to-bind-to-privileged-ports-1024-on-l#414258)):

    setcap 'cap_net_bind_service=+ep' /home/www/.cabal/bin/hogle

`relrod` on IRC says for port 80 usually we've been doing this (or at least I have for hl): Have the app server run and listen on localhost, then throw nginx front of it as a proxy. Let nginx handle things like SSL and caching (and binding to port 80 and 443). Not really a guide, but you can see the config in the ansible repo..

Alternatively, the solution that is running right now is:

	modprobe ip_tables
	echo 'ip_tables' >> /etc/modules
	iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to-ports 8080

Only the last line might actually be required. Based on http://unix.stackexchange.com/questions/10735/linux-allowing-an-user-to-listen-to-a-port-below-1024/10791#10791.
