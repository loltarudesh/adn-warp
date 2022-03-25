# Another Distributed Network

ADN is a collection of libraries and tools designed to create free autonomous networks.

The code documentation will soon be available on [hackage](https://hackage.haskell.org/)

You can visit the [wiki](../../wiki/) for more information about the project: goals, current state, outlooks.

We welcome gratefully any comment, reaction, question, suggestion or contribution to this project!

WARNING: this project is in a quite early stage. It has been very crudely tested, and should therefore be considered bugged, and used with caution.

## Test the program

### Installation

- clone this repository 
> git clone ""git@github.com:loltarudesh/adn-warp.git"
- if not present on your system, install the [haskell stack tool](https://docs.haskellstack.org/en/stable/README/)
> curl -sSL https://get.haskellstack.org/ | sh

### Execution

A wrapper script called `run_adn.sh` is used to launch the program.
It uses stack to download the dependencies and compile the program.

In order to restrict priviledged execution, this process requires two phases to be runned. a first stage is needed to generate internal configurations files, before actually running the program.

- First, the source code of the privillegied process is generated, so you can check its content
- Second, the program compiles theses process and runs the ADN process without any privillege in the ```scripts/``` repository. 

The privilleges are required to:

- create a tun interface
- set an IP address on both of the physical interface and the tun interface
- enable the IP routing and create the NAT rule (in case of a gateway daemon)

All the relevant parameters can be configured through comand line arguments.
Below are a few simple usage example. However, we tried to make this program highly parametrizable, and the option ```-h``` (```--help```) lists the available possibilities.


#### The gateway
> ./run_adn.sh genconf link-UDP --wlan-iface wlan0 --phy-ip 10.10.0.1 --warp --server --gateway eth0
>
> ./run_adn.sh run link-UDP --wlan-iface wlan0 --phy-ip 10.10.0.1 --warp --id 1 --server --gateway eth0

#### The client
> ./run_adn.sh genconf link-UDP --wlan-iface wlan0 --phy-ip 10.10.0.2 --warp --client
>
> ./run_adn.sh run link-UDP --wlan-iface wlan0 --phy-ip 10.10.0.2 --warp --id 2 --client

#### The relay
> ./run_adn.sh genconf link-UDP --wlan-iface wlan0 --phy-ip 10.10.0.3 --warp 
>
> ./run_adn.sh run link-UDP --wlan-iface wlan0 --phy-ip 10.10.0.3 --warp --id 3


#### Simulation

You can also simulate a network with many user. This is quite useless, but allows you to check out the project, without having to type your password!
> ./run_adn.sh run test chain 
> ./ run_adn.sh genconf test



## Autonomous networks

Every computer, phone or any device with Wifi or Bluetooth is in capacity to communicate with every other device in range, which could in turn relay data to any user in their range. By doing so, it seems possible to create entirely autonomous networks, without the need for any central authority or regulation.

Such networks have a wide range of application: allowing communication in isolated place, or in contrary in crowded areas where burst of communication can overwhelm a GSM relay. They would also offer very resilient way of communication against censorship and surveillance.

## Our goal
As its name imply, ADN is not at all the first project to look into distributed mesh networking (we must in particular cite the beautiful [Open-Mesh](https://www.open-mesh.org)). Many communication protocol have been proposed, implemented, and tested. If they have a hard time competing with more structured networks, they have proven many times that it is possible to communicate data, with a reasonable bitrate, through an autonomous network.


The core of ADN is a playground framework to design, implement and test new routing solution, and new distributed application.
It has been designed with simplicity has it's primary objective, and does not pretend to be efficient in any way. We tried to make it as user friendly as possible, and hope it to be flexible enough to support all the uses we though of, and maybe some of those we didn't think about. 

With this framework, we propose a rudimentary example, Warp. It is intenteded as a (barely) working proof-of-concept of a "free network", with:
- *self-addressing*: users can choose their address, without relying on a naming authority
- "self-routing": every user is in charge of the routing of it's own packets

At this stage, the only supported usage is tunneling internet traffic through a free network. We thought it to be a key feature, and a strong proof of feasability.
However, we believe the true interest of free networks lies in all the *new* use we have to invent, and encourage the development of application inside ADN.


## State of the project
This huge project can only be collective. With a though to [Conway's Law](https://en.wikipedia.org/wiki/Conway%27s_law), it seems clear that the best way to implement a distributed network is with a distributed project. The ADN core project aims at being a simple common framework, used by many other projects to build a full working network.

This approach is also imposed by the diversity of possible applications, leading to think that there is no good protocol, but many different ways of building networks, each adapted to its own situation. 


### Done and tested (possibly still bugged, however...)

* **[Core](../../wiki/adn-framework)**: a very simple framework designed to build network stack from independent modules and link them together, with naive implementation of logging, closing, CLI.
* **Debug**: tools to debug a stack
* **CLI**: not-so-rudimentary CLI, powered by [haskeline](https://hackage.haskell.org/package/haskeline)
* **Tun**: this module creates a virtual TUN interface, and allows to route ADN communication through it.
* **Link.UDP** open a simple UDP socket. Intended as a connection to a physical network (wifi-ibss and ethernet tested)
* **[Warp](../../wiki/warp)**: a very naive implementation, of a very naive routing protocol. Intended as an example! Allows to send a packet over a given road, a registers roads of passing-by packets.
* **Warp-search**: ask for a ressource in ADN, and retrieves road leading to it. 
* **IP tunneling**: route internet traffic over ADN, to a server. Main feature of this release

### To do
* **ADN tunneling** tunneling ADN communication through an IP link (ex: through a VPN)
* **ADN-chat** simple messaging service over ADN (Wrapper around IRC?)
* **IPV6** technical migration, should unlock many features!
* **Cryptography** not really needed at this stage, but certainly a major feature
* There is so much to do...

