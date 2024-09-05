**注意：最新のGNU Artanisのバージョンをインストールすることを推奨しますが、v0.6.1はまだリリースされていないため、ソースコードから最新バージョンをインストールする必要があります。**

### Guix

GNU Guixは先進的な機能的＆トランザクションパッケージマネージャーですので、ぜひ試してみてください。
[GNU Guixのインストール](https://guix.gnu.org/manual/en/html_node/Installation.html)をお読みください。

```bash
guix install artanis
```

### Ubuntu
Ubuntu 24.04でのみテストしています。他のLinuxディストリビューションでのテストを歓迎し、結果をお知らせください。

```bash
apt -qq -y install texinfo guile-3.0 guile-3.0-dev build-essential automake git autoconf libtool libmariadb-dev-compat libmariadb-dev libnss3 libnss3-dev

apt -qq -y install gettext redis redis-server libcurl4-openssl-dev
```

guile-dbi-2.1.8（tarballにはguile-dbdが含まれています）オプション

```bash
wget -c https://github.com/opencog/guile-dbi/archive/guile-dbi-2.1.8.tar.gz

tar xvzf guile-dbi-2.1.8.tar.gz
cd guile-dbi-guile-dbi-2.1.7/guile-dbi && ./autogen.sh --no-configure && ./configure && make
sudo make install
cd ../..
```

guile-dbd オプション。dbdプラグインは実際のデータベースサーバーに接続します。

```bash
cd guile-dbi-guile-dbi-2.1.8/guile-dbd-mysql && ./autogen.sh --no-configure && ./configure && make
sudo make install
```

このマニュアルの例ではMySQLを使用します。他のデータベース用のdbdプラグインはこちらで見つけることができます。インストール手順は同じです。

guile-curl（クライアントモジュール用）

```bash
git clone https://github.com/spk121/guile-curl.git
cd guile-curl && git checkout v0.9
./bootstrap && ./configure --prefix=/usr && make -j
make install
ln -s /usr/lib/guile/3.0/extensions/libguile-curl.* /usr/lib/
ldconfig
```

最新のGNU Artanisのインストール:

```bash
git clone https://gitlab.com/hardenedlinux/artanis.git
cd artanis
./autogen.sh --no-configure
./configure
make -j
sudo make install
```

## テスト

### 開発者向け
ChibaをQEMUベースのOpenBMCシミュレータでテストすることをお勧めします。以下の手順でシミュレータをセットアップします：

OpenBMCシミュレータの実行

```bash
- wget -c https://ozlabs.org/~joel/openbmc-qemu-tutorial/obmc-phosphor-image-romulus.static.mtd
- sudo apt install qemu-system-arm httpie
- bash scripts/run-openbmc-simulator.sh
```

Chibaの実行

```bash
cd chiba
art work
```

### ベンダー向け

実際の製品環境では、Chibaは使用される特定のOpenBMCファームウェアに合わせて調整され、適切なセキュリティ強化を組み込む必要があります。このカスタマイズ要件は、商業化の機会を開きます。

現在、実際のハードウェアはありません。もしあなたがハードウェアベンダーであれば、ぜひご連絡ください：NalaGinrut@hardenedlinux.org
