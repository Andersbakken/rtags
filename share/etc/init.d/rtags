#!/bin/bash
#
# chkconfig: 35 90 12
# description: Foo server
#

# Get function from functions library
. /etc/init.d/functions

# Start the service FOO
start() {
        initlog -c "echo -n Starting FOO server: "
        /path/to/FOO &
        ### Create the lock file ###
        touch /var/lock/subsys/FOO
        success $"FOO server startup"
        echo
}

# Restart the service FOO
stop() {
        initlog -c "echo -n Stopping FOO server: "
        killproc FOO
        ### Now, delete the lock file ###
        rm -f /var/lock/subsys/FOO
        echo
}

### main logic ###
case "$1" in
  start)
        start
        ;;
  stop)
        stop
        ;;
  status)
        status FOO
        ;;
  restart|reload|condrestart)
        stop
        start
        ;;
  *)
        echo $"Usage: $0 {start|stop|restart|reload|status}"
        exit 1
esac

exit 0
