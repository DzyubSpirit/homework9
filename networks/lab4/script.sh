sudo ifconfig enp3s0 192.168.5.1
sudo ifconfig enp3s0 hw ether 00:11:11:11:22:22
sudo ifconfig enp3s0 mtu 300
ping -c 5 -s 200 10.18.49.10
traceroute 10.18.50.1
ping samba.org
sudo tcpdump -i enp3s0 ip;tcp;udp;icmp

sudo tcpdump 'src net 0.0.0.0/0 and (tcp[tcpflags] & tcp-syn != 0 or (udp and net 10.18.51.0/24)) or (icmp and dst net 10.18.48.0/24)'
