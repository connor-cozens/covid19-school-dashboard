# Data Gatherer Node Deployment Recipe

For this example, assume we are configuring the OVH VPS server `vps-dcf4820e.vps.ovh.ca` 

1.   Log in to server as user "ubuntu" using the instructions in your OVH welcome email

`ssh ubuntu@vps-dcf4820e.vps.ovh.ca -p22`

2.   Change root password from default to strong one, record the password in the local password db

`passwd root`

3. allow root login via ssh 

`sudo sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config; sudo systemctl restart ssh`

4.   Deploy your ssh public key to ubuntu and root users on all new servers

```
ssh-copy-id ubuntu@vps-dcf4820e.vps.ovh.ca
ssh-copy-id root@vps-dcf4820e.vps.ovh.ca
```

5.   Add the new hosts to the ansible/production file (add them under the `[unconfigured_generic_server]` section). Here is an example:

```
[production_servers]


[unconfigured_generic_server]
shiny01 ansible_ssh_host=vps-dcf4820e.vps.ovh.ca ansible_port=22
```

6.   Update the OS on all new servers

```
ansible -i production -a "apt-get clean all" -u ubuntu --become unconfigured_generic_server
ansible -i production -m apt -a "update_cache=yes upgrade=dist autoremove=yes autoclean=yes" -u ubuntu --become unconfigured_generic_server
ansible -i production -a "apt-get -y autoremove" -u ubuntu --become unconfigured_generic_server
ansible -i production -a "apt-get -y autoclean" -u ubuntu --become unconfigured_generic_server
```

7.   Reboot all new servers

```
ansible -i production -m shell -a 'reboot' -u ubuntu --become unconfigured_generic_server
```

8.   Change server hostname to shiny_xx (as specified in the ansible/production inventory file) and add the new name to /etc/hosts

```
ansible-playbook playbooks/playbook-apply-inventory-hostname.yml -i production
```

9.   Install the OS packages required to support data gatherer functionality:

```
ansible-playbook playbooks/playbook-os-packages.yml -i production
```

10.   Start the NTP service

```
ansible -i production -m shell -a 'service ntp start && ntpq -p' -u ubuntu --become unconfigured_generic_server
```

11.   Set server timezone is set to Canada/Eastern

```
ansible-playbook playbooks/playbook-set-timezone.yml -i production
```

12.   Change SSH port from 22 to 63912

```
ansible-playbook playbooks/playbook-change-ssh-port.yml -i production
```

13.   Update ansible/production inventory file to reflect the new ssh port

```
[production_servers]


[unconfigured_generic_server]
shiny01 ansible_ssh_host=vps-dcf4820e.vps.ovh.ca ansible_port=63912
```

14.   Download, compile and configure ossec (see also: https://github.com/ossec/ossec-hids/issues/1663 and https://www.libellux.com/ossec/#troubleshooting). Perform the following commands on the server (TODO: automate this process with a playbook):

```
wget https://github.com/ossec/ossec-hids/archive/3.6.0.tar.gz
tar -xvzf 3.6.0.tar.gz
wget https://ftp.pcre.org/pub/pcre/pcre2-10.32.tar.gz
tar xzf pcre2-10.32.tar.gz -C ossec-hids-*/src/external
cd ossec-hids-3.6.0/
sudo PCRE2_SYSTEM=no ./install.sh
```

Use the following config options:

en
local
/var/ossec
y
peter.taylor@taypeinternational.com
y
y
y
y
y
n
ENTER

```
sudo service ossec start
sudo service ossec status
```

15. Configure ufw firewall on new nodes to allow access from administrator locations

```
ansible-playbook playbooks/playbook-configure-ufw.yml -i production
ansible -i production -m shell -a 'ufw status numbered' -u ubuntu --become unconfigured_generic_server
```

16.   Reboot all new servers

```
ansible -i production -m shell -a 'reboot' -u ubuntu --become unconfigured_generic_server
```

17. install shiny server

```
ansible -i production -m shell -a "R -e 'install.packages(\"shiny\", repos=\"https://cran.rstudio.com/\")' -u ubuntu --become unconfigured_generic_server
ansible -i production -m shell -a "wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.15.953-amd64.deb; sudo gdebi shiny-server-1.5.15.953-amd64.deb" -u ubuntu unconfigured_generic_server
ansible -i production -m shell -a "service shiny-server restart; service shiny-server status" -u ubuntu --become unconfigured_generic_server
```

18. install R packages required to support the school dashboard shiny web application

```
ansible-playbook playbooks/playbook-r-packages.yml -i production
ansible -i production -m shell -a "service shiny-server restart; service shiny-server status" -u ubuntu --become unconfigured_generic_server
```


18. update shiny configuration

19.   Test functionality

24.   If all is working as expected, modify the ansible/production inventory files so that the new nodes are moved from the `[unconfigured_generic_server]` section to the appropriate section in the production hosts inventory file