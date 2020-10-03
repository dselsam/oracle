# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Daniel Selsam

import socket
import sys
from Command_pb2 import Command
from handler import Handler

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
                cmd = Command()
                cmd.ParseFromString(msg)
                response = self.handler.handle(cmd)
                connection.sendall(response.SerializeToString())
            finally:
                connection.close()


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--port", action="store", dest='port', type=int, default=10000)
    opts = parser.parse_args()
    server = Server(Handler())
    server.launch(port=opts.port)
