
PROJ_NAME := testling

.PHONY: default

default:
	rebar compile

build:
	rebar ling-build

image:
	rebar ling-image

start:
	rsync vmling dom0::images/$(PROJ_NAME).img
	virsh -c xen+tcp://dom0 create $(PROJ_NAME).xml

console:
	virsh -c xen+tcp://dom0 console $(PROJ_NAME)

stop:
	virsh -c xen+tcp://dom0 destroy $(PROJ_NAME)

