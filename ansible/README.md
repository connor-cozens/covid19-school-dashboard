---
output: 
  html_document: 
    toc: yes
---

# Overview 

Useful ansible commands for managing Arbit cloud computing infrastructure

## install most revent ansible version

```
sudo add-apt-repository ppa:ansible/ansible
sudo apt-get update
sudo apt-get install ansible
```

# playbooks

## playbook: update all servers apt-get dist-upgrade

```
ansible-playbook ~/R/workspace/arbitrage-bot-production/ansible/playbooks/unconfigured-generic-server/playbook-production-update-os-all-servers.yml -i ~/R/workspace/arbitrage-bot-production/ansible/production
ansible-playbook ~/R/workspace/arbitrage-bot-production/ansible/playbooks/unconfigured-generic-server/playbook-production-update-os-all-servers.yml -i ~/R/workspace/arbitrage-bot-production/ansible/production-blocknet
```

## playbook: update config files on all data gatherer nodes

```
ansible-playbook ~/R/workspace/arbitrage-bot-production/ansible/playbooks/playbook-production-distribute-dg-config.yml -i ~/R/workspace/arbitrage-bot-production/ansible/production
```

## playbook: fix hostnames

```
ansible-playbook ~/R/workspace/arbitrage-bot-production/ansible/playbooks/playbook-apply-inventory-hostname.yml -i ~/R/workspace/arbitrage-bot-production/ansible/production
```

## playbook: create a user with sudo

http://stackoverflow.com/questions/37333305/ansible-create-a-user-with-sudo-privileges

# general

## shell: basic diagnostic commands

```
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m ping all
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "whoami" all
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "free -m" all
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "grep -c ^processor /proc/cpuinfo" all
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "cat /proc/cpuinfo | grep 'model name' | uniq" all
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "ifconfig" all
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "hostname -A" all
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "sudo cat /etc/hosts" all
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "df -h" all
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "sudo ufw status numbered" all
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "sudo service ossec restart && sudo service ossec status" all
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "Rscript -e 'R.Version()[[ \"version.string\" ]]'" all
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "sudo service shiny-server status" all
```

## shell: update production config on dg29 for br00t (blkbtc market making)

Remember to sftp upload updated copy of bot.config.dg29.blk.marketmaker.production to dg29 before proceeding!

```
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'cd ~/arbitrage-bot && git pull' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'cd ~/arbitrage-bot/config && rm bot.config && cp bot.config.dg29.blk.marketmaker.production bot.config' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'chmod a+w ~/arbitrage-bot/config/log' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'chmod a+w ~/arbitrage-bot/config/bot.config' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'chmod a+w ~/arbitrage-bot/data/resolved-redos.csv' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'chmod a+w ~/arbitrage-bot/webapp-simple-market-maker-generic' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'chmod a+w ~/arbitrage-bot/webapp-simple-market-maker-generic/*' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot/data/arbing-around-moving-averages-session-*' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot/data/arbing-around-moving-averages-session-WEB*' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot/data/arbing-around-moving-averages-session-ADA*' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot/data/arbing-around-moving-averages-session-BCC*' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot/data/arbing-around-moving-averages-session-BTC*' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot/data/arbing-around-moving-averages-session-BLK*' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot/data/arbing-around-moving-averages-session-DGB*' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot/data/arbing-around-moving-averages-session-DSH*' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot/data/arbing-around-moving-averages-session-ETH*' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot/data/arbing-around-moving-averages-session-LTC*' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot/data/arbing-around-moving-averages-session-XLM*' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot/data/arbing-around-moving-averages-session-XRP*' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot/webapp-arbing-around-ma' -u br00t dg29
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot/webapp-arbing-around-ma/*' -u br00t dg29

```

## shell: update production config on dg50 for br00t (dgxeth, dgxusd market making)

Remember to sftp upload updated copy of bot.config.dg50.dgx.marketmaker.production to dg50 before proceeding!

```
ansible -i ~/R/workspace/arbitrage-bot-bot-6/ansible/production -m shell -a 'cd ~/arbitrage-bot-bot-6 && git pull' -u br00t dg50
ansible -i ~/R/workspace/arbitrage-bot-bot-6/ansible/production -m shell -a 'cd ~/arbitrage-bot-bot-6/config && rm bot.config && cp bot.config.dg50.dgx.marketmaker.production bot.config' -u br00t dg50
ansible -i ~/R/workspace/arbitrage-bot-bot-6/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot-bot-6/config/log' -u br00t dg50
ansible -i ~/R/workspace/arbitrage-bot-bot-6/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot-bot-6/config/bot.config' -u br00t dg50
ansible -i ~/R/workspace/arbitrage-bot-bot-6/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot-bot-6/data/resolved-redos.csv' -u br00t dg50
ansible -i ~/R/workspace/arbitrage-bot-bot-6/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot-bot-6/webapp-simple-market-maker-generic' -u br00t dg50
ansible -i ~/R/workspace/arbitrage-bot-bot-6/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot-bot-6/webapp-simple-market-maker-generic/*' -u br00t dg50
ansible -i ~/R/workspace/arbitrage-bot-bot-6/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot-bot-6/data' -u br00t dg50
ansible -i ~/R/workspace/arbitrage-bot-bot-6/ansible/production -m shell -a 'sudo chmod a+w ~/arbitrage-bot-bot-6/data/*' -u br00t dg50
```

## shell: update production config on phobos for br00t (bccbtc, ethbtc market making)

Remember to sftp upload updated copy of bot.config.phobos.marketmaker.production to phobos before proceeding!

```
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'cd ~/R/workspace/arbitrage-bot-personal && git pull' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'cd ~/R/workspace/arbitrage-bot-personal/config && rm bot.config && cp bot.config.phobos.marketmaker.production bot.config' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/config/log' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/config/bot.config' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/data/resolved-redos.csv' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/data/resolved-redos.csv' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/webapp-simple-market-maker-generic' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/data/arbing-around-moving-averages-session-*' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/data/arbing-around-moving-averages-session-WEB*' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/data/arbing-around-moving-averages-session-BCC*' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/data/arbing-around-moving-averages-session-BSV*' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/data/arbing-around-moving-averages-session-BTC*' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/data/arbing-around-moving-averages-session-ETH*' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/data/arbing-around-moving-averages-session-LTC*' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/data/arbing-around-moving-averages-session-XLM*' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/data/arbing-around-moving-averages-session-XRP*' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/webapp-arbing-around-ma' -u br00t phobos
ansible -i ~/R/workspace/arbitrage-bot-personal/ansible/production -m shell -a 'sudo chmod a+w ~/R/workspace/arbitrage-bot-personal/webapp-arbing-around-ma/*' -u br00t phobos

```

## shell: git update arbitrage-bot and arbit-trade-data repos for all nodes

```
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'cd ~/arbitrage-bot && git pull' -u br00t data-gatherers-primary,data-gatherers-satellite,trading-engines
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'cd ~/arbit-trade-data && git pull' -u br00t trading-engines
```

## shell: update R packages on all data gatherers and trading engines

```
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "Rscript -e 'setRepositories(ind = 1:9); update.packages(lib.loc = \"/usr/local/lib/R/site-library\", ask = FALSE, repos = \"https://cloud.r-project.org/\")'" -u br00t data-gatherers-primary,data-gatherers-satellite,trading-engines

ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "sudo chown -Rf br00t:br00t /home/br00t/R/*" -u br00t data-gatherers-primary,data-gatherers-satellite,trading-engines

ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "Rscript -e 'setRepositories(ind = 1:9); update.packages(ask = FALSE, repos = \"https://cloud.r-project.org/\")'" -u br00t data-gatherers-primary,data-gatherers-satellite,trading-engines

ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "Rscript -e 'setRepositories(ind = 1:9); update.packages(ask = FALSE, repos = \"https://cloud.r-project.org/\")'" -u root data-gatherers-primary,data-gatherers-satellite,trading-engines

ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "sudo su - shiny; Rscript -e 'setRepositories(ind = 1:9); update.packages(lib.loc = \"/usr/lib/R/site-library\", ask = FALSE, repos = \"https://cloud.r-project.org/\")'" -u br00t trading-engines

ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a "sudo Rscript -e 'setRepositories(ind = 1:9); update.packages(lib.loc = \"/usr/lib/R/site-library\", ask = FALSE, repos = \"https://cloud.r-project.org/\")'" -u br00t data-gatherers-primary,data-gatherers-satellite,trading-engines

```

## shell: change git origin for arbitrage-bot and arbit-trade-data repos

```
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'cd ~/arbitrage-bot && git remote set-url origin ssh://git@dg30.arbit.ca:63912/arbit/arbitrage-bot.git' -u br00t data-gatherers-primary,data-gatherers-satellite,trading-engines
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'cd ~/arbit-trade-data && git remote set-url origin ssh://git@dg30.arbit.ca:63912/br00t/arbit-trade-data.git' -u br00t trading-engines
```

## shell: update all pcs (apt-get dist-upgrade) in calgary-office

```
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/calgary-office -m apt -a "update_cache=yes upgrade=dist autoremove=yes autoclean=yes" --sudo all
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/calgary-office -a "apt-get -y autoremove" --sudo all
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/calgary-office -a "apt-get -y autoclean" --sudo all
```

## shell: fix permissions that got broken on upgrade to R version 3.4.1

```
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo usermod -a -G staff br00t && sudo chown -Rf root:staff /usr/lib/R/site-library && sudo chown -Rf root:staff /usr/lib/R/library && sudo chown root:staff -Rf /usr/local/lib/R/site-library' data-gatherers-primary,data-gatherers-satellite,trading-engines
```

## shell: on data-gatherers-primary, we do not need the ssh tunnel so we need to slightly modify our arbit-dg init script (comment out the tunnel.sh lines)

```
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'cd ~/arbitrage-bot && git pull' -u br00t data-gatherers-primary
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo cp arbitrage-bot/scripts/bash/arbit-dg /etc/init.d/arbit-dg && sudo chmod +x /etc/init.d/arbit-dg &&  sudo update-rc.d arbit-dg defaults 100' -u br00t data-gatherers-primary
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t data-gatherers-primary
```

## shell: restart satellite data gatherers in sequence 

```
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t dg43
sleep 180
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t dg51
sleep 180
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t dg26
sleep 180
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t dg12
sleep 180
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t dg01
sleep 180
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t dg02
sleep 180
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t dg03
sleep 180
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t dg04
sleep 180

```

## shell: restart all data gatherers and trading engines in sequence (remember to wait long engough for data-gatherers-primary to fully start)

```
# ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'killall R; killall python3' -u br00t trading-engines
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'killall R; killall python3' -u br00t dg29,dg50,phobos
sleep 30
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t data-gatherers-primary
sleep 500
# ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sleep 10; sudo reboot' -u br00t trading-engines
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sleep 10; sudo reboot' -u br00t dg29,dg50,phobos
sleep 30
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t data-gatherers-satellite
sleep 900

```

## shell: restart other servers

```
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t gitlab # dg30
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t sftp # dg31
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t rocketchat # dg32 
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t balances-dashboard # dg33
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t dg40 # intranet dashboard
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t mumble # dg41
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t dg45 # dg45
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t dg42 # trade downloader
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo reboot' -u br00t arbit-wb-01 # web site

```

# firewall

## shell: ansible firewall modification template

```
ansible -i ~/R/workspace/arbitrage-bot-production/ansible/production -m shell -a 'sudo ufw allow from <IP-HERE> to any port 63912 comment "... added xxxx-xx-xx"' -u br00t <SERVER-NAME-HERE>
```

See also `sandbox/firewall-rules-ansible.R` script.