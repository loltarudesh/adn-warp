#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>


// usage : ./configure_client <delete | set ip_addr mtu>
//      SCRIPT_DESCRIPTION 


int print_error(int argc, char** argv);

int set_tun(int argc, char** argv){ 
    if(argc < 4) return print_error(argc,argv);
    else {
        char cmd[256];
        setuid(0); // gaining root rights
        system("ip link set ADN_IFACE_NAME down");
        system("ip addr flush ADN_IFACE_NAME");
        sprintf(cmd, "ip link set ADN_IFACE_NAME mtu %s", argv[3]); system(cmd); // TODO prevent injection
        system("ip link set ADN_IFACE_NAME up");
        sprintf(cmd, "ip addr add %s/24 dev ADN_IFACE_NAME", argv[2]); system(cmd);
        system("ip route add default dev ADN_IFACE_NAME");
        return 0;
    }
}

int delete_tun(){
    setuid(0); // gaining root rights
    system("ip link set ADN_IFACE_NAME down");
    system("ip addr flush ADN_IFACE_NAME");
    return 0;
}

int main(int argc, char** argv){
    if( argc < 2) return print_error(argc,argv);
    else {
        if(0==strcmp(argv[1],"set")) return set_tun(argc,argv);
        else if(0==strcmp(argv[1], "delete")) return delete_tun();
        else {
            printf("%s => unrecognized command: %s\n", argv[0],argv[1]);
            return -1;
        }
    }
}

int print_error(int argc, char** argv){
    printf("%s => this program require more arguments. Received:", argv[0]);
    for(int i=1; i<argc; i++)
        printf(" %s",argv[i]);
    printf("\n");
    return -1;
}

