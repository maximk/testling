
PROJ_NAME := testling

.PHONY: default

default:
	rebar compile

build:
	rebar compile
	rebar ling-build

image:
	rebar ling-image

me:
	rebar compile
	rebar ling-build-image

em:
	rebar compile
	rebar ling-build-image

start:
	rsync vmling dom0::images/$(PROJ_NAME).img
	virsh -c xen+tcp://dom0 create --console $(PROJ_NAME).xml

console:
	virsh -c xen+tcp://dom0 console $(PROJ_NAME)

stop:
	virsh -c xen+tcp://dom0 destroy $(PROJ_NAME)

