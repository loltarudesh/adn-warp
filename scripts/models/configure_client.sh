#!/usr/bin/env sh

ACTION=$1
IP_ADDR=$2
MTU=$3

set -x
case $ACTION in

    set)
        sudo ip link set ADN_IFACE_NAME down
        sudo ip addr flush ADN_IFACE_NAME
        sudo ip link set ADN_IFACE_NAME mtu $MTU
        sudo ip link set ADN_IFACE_NAME up
        sudo ip addr add $IP_ADDR/24 dev ADN_IFACE_NAME
        sudo ip route add default dev ADN_IFACE_NAME
        ;;

    delete)
        sudo ip link set ADN_IFACE_NAME down
        sudo ip addr flush ADN_IFACE_NAME
        ;;
    *)
        echo "Error parsing command..."
esac

