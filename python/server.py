import socket
import sys
import protos.Command_pb2 as Command
import handler

class Server:
    def __init__(self, handler):
        self.handler = handler

    def launch(self, port):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_address = ('localhost', port)
        print('starting up on {} port {}'.format(*server_address))
        sock.bind(server_address)

        sock.listen(1)

        while True:
            connection, client_address = sock.accept()
            try:
                print('connection from', client_address)
                msg = connection.recv(100000)
                print('received %d bytes' % len(msg))
                cmd = Command.Command()
                cmd.ParseFromString(msg)
                self.handler.handle(cmd)
                connection.sendall(cmd.SerializeToString())
            finally:
                connection.close()

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--port", action="store", dest='port', type=int, default=10000)
    opts = parser.parse_args()
    server = Server(handler.Handler())
    server.launch(port=opts.port)
