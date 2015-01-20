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
	export PATH=/home/www/.cabal.bin:/opt/ghc/7.8.4/bin:/opt/cabal/1.18/bin:/opt/happy/1.19.4/bin:/opt/alex/3.1.3/bin:$PATH
	git pull
	cabal update
	cabal install
	hogle gen
	hogle server --port=8080 &

These commands are also stored as `/home/www/update.sh`.

## Monitoring

* `df -h`, check there is sufficient disk space.
* `top`, see what is running.
