# Just a default xinitrc that we use on our current worker machines It leaves
# you with a useable xterm, and one xterm that shows the benchmark control
# output. On our HPI machine, we need to ssh into the API server to be able to
# receive jobs

cd "`dirname $(readlink $0)`"
xterm &
if grep hpi.uni-potsdam.de /etc/resolv.conf > /dev/null 2>&1; then
    # Within HPI, we must make ourselves known to the API. Try to reconnect if we get disconnected.
    bash -c "while true; do xterm -e ssh -R8083:localhost:8083 dglaeser@172.16.64.134 'pwd; cd ~/RSqueakBenchmarks && git pull && python benchmark-api.py'; sleep 30; done" &
fi
xterm -e python benchmark-control.py &
twm
