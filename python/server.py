import socket
import sys
import json

class Server:
    def __init__(self, handler):
        self.handler = handler

    def handle(self, msg):
        msg = msg.decode("utf-8")
        cmd, msg = msg.split(" ", maxsplit=1)
        if cmd == "train":
            self.handler.train(opts=json.loads(msg))
        elif cmd == "predict":
            self.handler.predict(query_str=msg)
        elif cmd == "save-weights":
            self.handler.save_weights(filename=msg)
        elif cmd == "load-weights":
            self.handler.load_weights(filename=msg)
        elif cmd == "add-replay":
            # in this jargon, 'datapoint_str' includes the scores at the beginning
            # while 'query_str' does not.
            filename, datapoint_str = msg.split(" ", maxsplit=1)
            self.handler.add_replay(filename=filename, datapoint_str=datapoint_str)
        else:
            raise Exception("unexpected command: %s" % cmd)

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
                result = self.handle(msg)
                connection.sendall(result)
            finally:
                connection.close()

class SequenceHandler:
    def train(self, opts):                         raise Exception("NYI")
    def predict(self, query_str):                  raise Exception("NYI")
    def save_weights(self, filename):              raise Exception("NYI")
    def load_weights(self, filename):              raise Exception("NYI")
    def add_replay(self, filename, datapoint_str): raise Exception("NYI")


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--port", action="store", dest='port', type=int, default=10000)
    opts = parser.parse_args()
    server = Server(SequenceHandler())
    server.launch(port=opts.port)
