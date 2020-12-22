---
output: 
  html_document: 
    toc: yes
---

# Overview 

Useful ansible commands for managing covid19schooldashboard cloud computing infrastructure

## install most revent ansible version

```
sudo add-apt-repository ppa:ansible/ansible
sudo apt-get update
sudo apt-get install ansible
```

# playbooks

## playbook: update all servers apt-get dist-upgrade

```
ansible-playbook playbooks/playbook-update-os.yml -i production
```

# general

## shell: basic diagnostic commands

```
ansible -i production -m ping -u ubuntu all
ansible -i production -m shell -a "whoami" -u ubuntu all
ansible -i production -m shell -a "free -m" -u ubuntu all
ansible -i production -m shell -a "grep -c ^processor /proc/cpuinfo" -u ubuntu all
ansible -i production -m shell -a "cat /proc/cpuinfo | grep 'model name' | uniq" -u ubuntu all
ansible -i production -m shell -a "ifconfig" -u ubuntu all
ansible -i production -m shell -a "hostname -A" -u ubuntu all
ansible -i production -m shell -a "cat /etc/hosts" --become -u ubuntu all
ansible -i production -m shell -a "df -h" -u ubuntu all
ansible -i production -m shell -a "ufw status numbered" --become -u ubuntu all
ansible -i production -m shell -a "service ossec restart && service ossec status" --become -u ubuntu all
ansible -i production -m shell -a "Rscript -e 'R.Version()[[ \"version.string\" ]]'" -u ubuntu all
ansible -i production -m shell -a "service shiny-server status" --become -u ubuntu all
```


## shell: update R packages on all data gatherers and trading engines

```
ansible -i production -m shell -a "Rscript -e 'setRepositories(ind = 1:9); update.packages(ask = FALSE, repos = \"https://cloud.r-project.org/\")'" -u ubuntu production_servers
```

# firewall

## shell: ansible firewall modification template

```
ansible -i production -m shell -a 'sudo ufw allow from <IP-HERE> to any port 63912 comment "... added xxxx-xx-xx"' -u ubuntu <SERVER-NAME-HERE>
```