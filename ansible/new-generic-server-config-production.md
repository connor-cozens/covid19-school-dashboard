# Data Gatherer Node Deployment Recipe

For this example, assume we are configuring the OVH VPS server `vps-dcf4820e.vps.ovh.ca` 

1. Deploy your ssh public key to ubuntu user on all new servers (using the credentials in your OVH welcome email)

```
ssh-copy-id ubuntu@vps-dcf4820e.vps.ovh.ca
```

2. Test log in to server as user "ubuntu" 

```
ssh ubuntu@vps-dcf4820e.vps.ovh.ca -p22
```

3. Add all new hosts you want to configure to the ansible/production file (add them under the `[unconfigured_generic_server]` section). Here is an example:

```
[production_servers]


[unconfigured_generic_server]
shiny01 ansible_ssh_host=vps-dcf4820e.vps.ovh.ca ansible_port=22
```

4. Update the OS on all new servers

```
ansible -i production -a "apt-get clean all" -u ubuntu --become unconfigured_generic_server
ansible -i production -m apt -a "update_cache=yes upgrade=dist autoremove=yes autoclean=yes" -u ubuntu --become unconfigured_generic_server
ansible -i production -a "apt-get -y autoremove" -u ubuntu --become unconfigured_generic_server
ansible -i production -a "apt-get -y autoclean" -u ubuntu --become unconfigured_generic_server
```

5. Reboot all new servers

```
ansible -i production -m shell -a 'reboot' -u ubuntu --become unconfigured_generic_server
```

6. Change server hostname to shiny_xx (as specified in the ansible/production inventory file) and add the new name to /etc/hosts

```
ansible-playbook playbooks/playbook-hostname.yml -i production
```

7. Install the OS packages required to support data gatherer functionality:

```
ansible-playbook playbooks/playbook-packages-os.yml -i production
```

8. Start the NTP service

```
ansible -i production -m shell -a 'service ntp start && ntpq -p' -u ubuntu --become unconfigured_generic_server
```

9. Set server timezone is set to Canada/Eastern

```
ansible-playbook playbooks/playbook-timezone.yml -i production
```

10. Change SSH port from 22 to 63912

```
ansible-playbook playbooks/playbook-ssh.yml -i production
```

11. Update ansible/production inventory file to reflect the new ssh port

```
[production_servers]


[unconfigured_generic_server]
shiny01 ansible_ssh_host=vps-dcf4820e.vps.ovh.ca ansible_port=63912
```

12. Download, compile and configure ossec (see also: https://github.com/ossec/ossec-hids/issues/1663 and https://www.libellux.com/ossec/#troubleshooting). Perform the following commands on the server (TODO: automate this process with a playbook):

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

13. Configure ufw firewall on new nodes to allow access from administrator locations

```
ansible-playbook playbooks/playbook-ufw.yml -i production
ansible -i production -m shell -a 'ufw status numbered' -u ubuntu --become unconfigured_generic_server
```

14. Reboot all new servers

```
ansible -i production -m shell -a 'reboot' -u ubuntu --become unconfigured_generic_server
```

15. install shiny server

```
ansible -i production -m shell -a "R -e 'install.packages(\"shiny\", repos=\"https://cran.rstudio.com/\")' -u ubuntu --become unconfigured_generic_server
ansible -i production -m shell -a "wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.15.953-amd64.deb; sudo gdebi shiny-server-1.5.15.953-amd64.deb" -u ubuntu unconfigured_generic_server
ansible -i production -m shell -a "service shiny-server restart; service shiny-server status" -u ubuntu --become unconfigured_generic_server
```

16. install R packages required to support the school dashboard shiny web application

```
ansible-playbook playbooks/playbook-packages-r.yml -i production
ansible -i production -m shell -a "service shiny-server restart; service shiny-server status" -u ubuntu --become unconfigured_generic_server
```

17. update shiny configuration

18. Test functionality

19. If all is working as expected, modify the ansible/production inventory files so that the new nodes are moved from the `[unconfigured_generic_server]` section to the appropriate section in the production hosts inventory file