# Just a default xinitrc that we use on our current worker machines It leaves
# you with a useable xterm, and one xterm that shows the benchmark control
# output. On our HPI machine, we need to ssh into the API server to be able to
# receive jobs

xterm &
if grep hpi.uni-potsdam.de /etc/resolv.conf > /dev/null 2>&1; then
    # Within HPI, we must make ourselves known to the API
    xterm -e ssh -R8083:localhost:8083 dglaeser@172.16.64.134 &
fi
cd "`dirname $(readlink $0)`"
xterm -e python benchmark-control.py
twm
