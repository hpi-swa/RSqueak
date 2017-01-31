# Just a default xinitrc that we use on our current worker machines It leaves
# you with a useable xterm, and one xterm that shows the benchmark control
# output. On our HPI machine, we need to ssh into the API server to be able to
# receive jobs

cd "`dirname $(readlink $0)`"
xterm &
if grep hpi.uni-potsdam.de /etc/resolv.conf > /dev/null 2>&1; then
    # Within HPI, we must make ourselves known to the API. Try to reconnect if we get disconnected.
    xterm -e "bash -c 'while true; do ssh -Rlocalhost:9093:localhost:8082 -N lively-kernel.org; sleep 30; done'" &
fi
xterm -e python benchmark-control.py &
twm
