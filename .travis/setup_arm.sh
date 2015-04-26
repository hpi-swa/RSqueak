set -ex
sudo apt-get update

# install arm environment
sudo apt-get install -y debootstrap schroot binfmt-support qemu-system qemu-user-static scratchbox2 gcc-arm-linux-gnueabi libsdl1.2-dev libffi-dev
mkdir precise_arm
sudo chown $USER /etc/schroot/schroot.conf
echo "
[precise_arm]
directory=$PWD/precise_arm
users=$USER
root-users=$USER
groups=$USER
aliases=default
type=directory
" >>  /etc/schroot/schroot.conf
cat /etc/schroot/schroot.conf
sudo chown root /etc/schroot/schroot.conf

sudo qemu-debootstrap --variant=buildd --arch=armel precise precise_arm/ http://ports.ubuntu.com/ubuntu-ports/
schroot -c precise_arm -- uname -m
sudo su -c 'echo "deb http://ports.ubuntu.com/ubuntu-ports/ precise main universe restricted" > precise_arm/etc/apt/sources.list'
schroot -c precise_arm -u root -- apt-get update
schroot -c precise_arm -u root -- apt-get install -y libffi-dev python-dev build-essential libsdl1.2-dev
cd precise_arm/
sb2-init -c `which qemu-arm` ARM `which arm-linux-gnueabi-gcc`
cd ..
