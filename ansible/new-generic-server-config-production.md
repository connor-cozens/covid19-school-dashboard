# Multipass virtual instance setup

1. install multipass (https://snapcraft.io/multipass)

```
sudo snap install multipass --classic --beta  
```

2. update the multipass-instance-config.yaml to include your ssh public key

3. launch multipass instance to test this recipe

```
multipass stop covid19schooldash
multipass delete covid19schooldash
multipass purge
multipass launch --cloud-init multipass-instance-config.yaml --name covid19schooldash --disk 50G --mem 4G
multipass info covid19schooldash
```

# Shiny web application server deployment recipe

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

4. Make user "ubuntu" passwordless sudoer

```
ansible-playbook -i production --ask-become-pass playbooks/playbook-sudoers.yml
```

5. Update the OS on all new servers

```
ansible-playbook playbooks/playbook-update-os.yml -i production
```

6. Change server hostname to shiny_xx (as specified in the ansible/production inventory file) and add the new name to /etc/hosts

```
ansible-playbook playbooks/playbook-hostname.yml -i production
```

7. Install the OS packages required to support dashboard functionality:

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
admin@covid19schooldashboard.com
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

13. Configure ufw firewall on new nodes to allow access from administrator locations (don't do this in multipass test environment!)

```
ansible-playbook playbooks/playbook-ufw.yml -i production
ansible -i production -m shell -a 'ufw status numbered' -u ubuntu --become unconfigured_generic_server
```

14. install shiny server

```
ansible -i production -m shell -a "wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.15.953-amd64.deb; gdebi -n shiny-server-1.5.15.953-amd64.deb" -u ubuntu --become unconfigured_generic_server
ansible -i production -m shell -a "service shiny-server restart; service shiny-server status" -u ubuntu --become unconfigured_generic_server
```

15. install R packages required to support the school dashboard shiny web application

```
ansible-playbook playbooks/playbook-packages-r.yml -i production
ansible -i production -m shell -a "service shiny-server restart; service shiny-server status" -u ubuntu --become unconfigured_generic_server
```

16. check out source code for covid19 dashboard application from gitlab

```
ansible -i production -m shell -a 'cat /dev/zero | ssh-keygen -q -N ""; echo; echo; cat /home/ubuntu/.ssh/id_rsa.pub' -u ubuntu unconfigured_generic_server
```

take the public keys and add them to the ssh keys for your user on gitlab

```
ansible -i production -m shell -a 'ssh-keyscan gitlab.com >> ~/.ssh/known_hosts' -u ubuntu unconfigured_generic_server
ansible -i production -m shell -a 'rm -Rf ~/ontario-covid19-dashboard; env GIT_SSL_NO_VERIFY=true git clone git@gitlab.com:br00t/ontario-covid19-dashboard.git' -u ubuntu unconfigured_generic_server
```

16. update shiny configuration

```
ansible-playbook playbooks/playbook-shiny-server.yml -i production
```

17. Inject google maps api key into data_downloader.R script

```
ansible-playbook playbooks/playbook-google-api-key.yml -i production
```

18. Reboot all new servers

```
ansible -i production -m shell -a 'reboot' -u ubuntu --become unconfigured_generic_server
```

19. Test functionality

Visit the following page in a web browser to ensure the dashboard is functional: http://vps-dcf4820e.vps.ovh.ca/

19. If all is working as expected, modify the ansible/production inventory files so that the new nodes are moved from the `[unconfigured_generic_server]` section to the appropriate section in the production hosts inventory file

# Alternate Approach

Containerize the application and deploy to kubernetes cluster(https://www.statworx.com/at/blog/how-to-dockerize-shinyapps/, https://rstudio.github.io/renv/articles/renv.html, https://towardsdatascience.com/deploy-rshiny-on-kubernetes-with-a-helm-chart-fa07e4572942)

1. Build docker image of shiny app

```
cd ~/R/workspace/experimental-code/covid19-ontario-schools
sudo DOCKER_BUILDKIT=1 docker build --ssh default=~/.ssh/covid19schooldashboard/id_rsa . # build with ssh agent to forward keys into machine
```

2. test the image, this will launch the image attached so you can see console output from the shiny app, visit web browser http://localhost:3838 to test the web app

```
sudo docker run -p localhost:3838:3838 covid19schooldashboard
```

alternately you can launch the image detached:

```
sudo docker run -d -p localhost:3838:3838 covid19schooldashboard
sudo docker ps
```

2. Push the docker image into docker hub per https://ropenscilabs.github.io/r-docker-tutorial/04-Dockerhub.html, you will need dockerhub login credentials

```
sudo docker login --username=br00t
sudo docker images
docker tag <IMAGE_ID> br00t/covid19schooldashboard:<TAG>
docker push br00t/covid19schooldashboard:<TAG>
```

3. create OVH kubernetes cluster and obtain the kubeconfig.yml file per https://docs.ovh.com/ca/en/kubernetes/creating-a-cluster/

4. create registry credentials per: 
https://stackoverflow.com/questions/49669077/helm-chart-deployment-and-private-docker-repository
https://helm.sh/docs/howto/charts_tips_and_tricks/#creating-image-pull-secrets
https://kubernetes.io/docs/tasks/configure-pod-container/pull-image-private-registry/

```
kubectl create secret docker-registry regcred --docker-server=<your-registry-server> --docker-username=<your-name> --docker-password=<your-pword> --docker-email=<your-email>
```

3. Use `helm` to deploy the application to OVH kubernetes cluster 

```
helm --kubeconfig kubeconfig.yml dep up covid19
helm --kubeconfig kubeconfig.yml uninstall covid19
helm --kubeconfig kubeconfig.yml upgrade --install covid19 ./covid19
kubectl --kubeconfig kubeconfig.yml get pods
kubectl --kubeconfig kubeconfig.yml get svc
```


