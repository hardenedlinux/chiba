**NOTE: It is recommended to install the latest version of GNU Artanis, however, since the v0.6.1 hasen't been released yet, you can install the latest version from the source code.**

### Guix

GNU Guix is the advanced functional & transactional package manager, we recommend you to try it.
You may read the [GNU Guix installation](https://guix.gnu.org/manual/en/html_node/Installation.html)

```bash
guix install artanis
```

### Ubuntu
We only tested it on Ubuntu 24.04. Welcome to test it on other Linux distributions, and please let us know the result.

```bash
apt -qq -y install texinfo guile-3.0 guile-3.0-dev build-essential automake git autoconf libtool libmariadb-dev-compat libmariadb-dev libnss3 libnss3-dev

apt -qq -y install gettext redis redis-server libcurl4-openssl-dev
```

#### guile-dbi-2.1.8 (The tarball included guile-dbd) *[Optional]*
```bash
wget -c https://github.com/opencog/guile-dbi/archive/guile-dbi-2.1.8.tar.gz

tar xvzf guile-dbi-2.1.8.tar.gz
cd guile-dbi-guile-dbi-2.1.7/guile-dbi && ./autogen.sh --no-configure && ./configure && make
sudo make install
cd ../..
```

#### guile-dbd *[Optional]*. The dbd plugins connect to an actual database server.
```bash
cd guile-dbi-guile-dbi-2.1.8/guile-dbd-mysql && ./autogen.sh --no-configure && ./configure && make
sudo make install
```
MySQL is used for the examples in this manual. You may find dbd plugins for other databases
[[ https://github.com/opencog/guile-dbi][here]]. The installation process is identical.

#### guile-curl for the client module.
```bash
git clone https://github.com/spk121/guile-curl.git
cd guile-curl && git checkout v0.9
./bootstrap && ./configure --prefix=/usr && make -j
make install
ln -s /usr/lib/guile/3.0/extensions/libguile-curl.* /usr/lib/
ldconfig
```
**NOTE:** You may need to make necessary soft links according to above script.

#### guile-redis:
```bash
git clone https://github.com/aconchillo/guile-redis.git
cd guile-redis
git checkout -b 2.2.0
autoreconf -vif
./configure --prefix=/usr
make
make install
```

#### Install the latest GNU Artanis:*

```bash
git clone https://gitlab.com/hardenedlinux/artanis.git
cd artanis
./autogen.sh --no-configure
./configure
make -j
sudo make install
```

## Test

### For developers

It is recommended to test Chiba on a QEMU-based OpenBMC simulator. The following steps show how to set up the simulator:

#### Run OpenBMC simulator

```bash
- wget -c https://ozlabs.org/~joel/openbmc-qemu-tutorial/obmc-phosphor-image-romulus.static.mtd
- sudo apt install qemu-system-arm httpie
- bash scripts/run-openbmc-simulator.sh
```

#### Run Chiba

```bash
cd chiba
art work
```

### For vendors

In real-world product environments, Chiba must be tailored to align with the specific OpenBMC firmware in use, incorporating appropriate security enhancements. This customization requirement opens up opportunities for commercialization.

For now, we don't have real hardware to test. If you are a hardware vendor, please contact us: NalaGinrut@hardenedlinux.org.
