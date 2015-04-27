set -ex
sudo apt-get update

# install arm environment
sudo apt-get install -y debootstrap schroot binfmt-support qemu-system qemu-user-static scratchbox2 gcc-arm-linux-gnueabihf libsdl1.2-dev libffi-dev
mkdir raspbian_arm
sudo chown $USER /etc/schroot/schroot.conf
echo "
[raspbian_arm]
directory=$PWD/raspbian_arm
users=$USER
root-users=$USER
groups=$USER
aliases=default
type=directory
" >>  /etc/schroot/schroot.conf
cat /etc/schroot/schroot.conf
sudo chown root /etc/schroot/schroot.conf

wget http://archive.raspbian.org/raspbian.public.key
sudo apt-key add raspbian.public.key

sudo qemu-debootstrap --variant=buildd --keyring /etc/apt/trusted.gpg --arch armhf --foreign wheezy\
   raspbian_arm/ http://archive.raspbian.org/raspbian/
schroot -c raspbian_arm -- uname -m
sudo su -c 'echo "deb http://archive.raspbian.org/raspbian/ wheezy main universe restricted" > raspbian_arm/etc/apt/sources.list'
schroot -c raspbian_arm -u root -- apt-get update
schroot -c raspbian_arm -u root -- apt-get install -y libffi-dev build-essential libsdl1.2-dev # python-dev 
cd raspbian_arm/
sb2-init -c `which qemu-arm` ARM `which arm-linux-gnueabihf-gcc`
cd ..
