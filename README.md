RESTapi
=======
This is a bare bones project that includes a very simple database interface for a collection of Mnesia tables.

The intent of this project is to provide a framework to quickly build a small HTTPrest API on top of Mnesia.

Follow these steps to complete the tutorial.

Please note that we'll be doing some things you might not want in production as purely educational exercises.

Setup Guide
===========
#### Dowload Project Stucture

- `git clone https://github.com/austinerlang/RESTapi`

#### Setup Virtual Dev Environment

##### DOCKER

Make sure you have docker [installed](https://docs.docker.com/installation/)

TO DO - We are working on a docker file for this. You can test this:

`sudo docker run -it -p 8080:8080 austinerlang/restapi /bin/bash`

##### VAGRANT

Make sure you have vagrant [installed](http://docs.vagrantup.com/v2/installation/)

- `vagrant up`
- `vagrant ssh`

#### Mnesia DB

Verify Mnesia built the schema files correctly.

- `ls -l /var/rest_db/`

If the directory is empty, there is a bug with vagrant / erlang shell during post provisioning. Manually fix:

- `cd /vagrant/rest`
- `rebar3 release`
- `erl -pa _build/default/lib/*/ebin -sname rest -mnesia dir '"/var/rest_db"' -run rest_db initial_setup -s init stop`

Now verify the files exist in /var/rest_db.

#### Tutorial

Follow along with us at the meetup!!!