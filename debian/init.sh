
#pkgs
sudo apt-get install tig zsh git-core build-essential curl wget w3m tmux screen
curl -L https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh | sh
sudo chsh -s /usr/bin/zsh
echo 'export PATH="$PATH:$HOME/bin"' >> ~/.zshrc
cp ../git/.git* ~/


#emacs
wget -q -O - http://emacs.naquadah.org/key.gpg | sudo apt-key add -
echo 'deb http://emacs.naquadah.org/ stable/' | sudo tee -a /etc/apt/sources.list
echo 'deb-src http://emacs.naquadah.org/ stable/' | sudo tee -a /etc/apt/sources.list
sudo apt-get update
sudo apt-get install emacs-snapshot
mkdir ~/.emacs.d
cp ../emacs/emacs-conf/init.el ~/.emacs.d/init.el
emacs --daemon

#java, clojure
sudo apt-get install openjdk-7-jdk
mkdir ~/bin
wget https://raw.github.com/technomancy/leiningen/stable/bin/lein -O ~/bin/lein
chmod +x ~/bin/lein
~/bin/lein upgrade

#lua
sudo apt-get install lua5.2 luarocks



