# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "ubuntu/trusty32"

  # provision build environment
  config.vm.provision :shell, inline: <<SCRIPT
apt-get update
apt-get install -y pypy python-pytest libsdl-dev libffi-dev git
SCRIPT

  # provision dependencies with the script for travis
  config.vm.provision :shell, inline: <<SCRIPT
apt-get install -y dos2unix
mkdir /home/vagrant/deps
chown vagrant /home/vagrant/deps
cd /home/vagrant/deps
cp /vagrant/.travis/install_requirements.sh inst.sh
dos2unix inst.sh
./inst.sh
chown -R vagrant /home/vagrant/deps

cd /home/vagrant
APP_PYTHONPATH='export PYTHONPATH=$PYTHONPATH:~/deps/pypy-pypy:~/deps/pypy-rsdl'
if ! grep "$APP_PYTHONPATH" .bashrc >/dev/null 2>/dev/null; then
	echo $APP_PYTHONPATH >> .bashrc
fi
SCRIPT

  # provision graphical UI and convenience tools
  config.vm.provision :shell, inline: <<SCRIPT
apt-get install -y python-pygame graphviz fluxbox nodm xinit vim-athena xclip xterm

# set up nodm to login the vagrant user
sed -i s/NODM_ENABLED=false/NODM_ENABLED=true/ /etc/default/nodm
sed -i s/^NODM_USER=.*$/NODM_USER=vagrant/ /etc/default/nodm

ps -C nodm >/dev/null || service nodm start
# start an xterm with fluxbox
if ! grep 'xterm &' /home/vagrant/.fluxbox/startup >/dev/null 2>/dev/null; then
	sleep 2
	sed -i '/^exec fluxbox$/ixterm &' /home/vagrant/.fluxbox/startup
	service nodm stop
	sleep 2
	service nodm start
fi
SCRIPT

  config.vm.network "forwarded_port", guest: 5000, host:8085

  config.vm.provider "virtualbox" do |vb|
	  # Don't boot with headless mode
	  vb.gui = true

	  vb.customize ["modifyvm", :id, "--memory", "2048"]
  end
end
