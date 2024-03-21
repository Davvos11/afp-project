import zmq
import asyncio
import websockets
from websockets.server import serve

# We did not find a good ZeroMQ library in Haskell. Instead this simple script
# creates a WebSocket server which relays the message

context = zmq.Context()

print("Connecting to ZeroMQ socket of NDOV loket...")
socket = context.socket(zmq.SUB)
socket.connect("tcp://pubsub.besteffort.ndovloket.nl:7658")

envelopes = [
    "/ARR/KV15messages",     # (Arriva)
    "/ARR/KV17cvlinfo",
    "/ARR/KV6posinfo",
    "/CXX/KV15messages",     # (Connexxion, Breng, OV Regio IJsselmond)
    "/CXX/KV17cvlinfo",
    "/CXX/KV6posinfo",
    "/DITP/KV15messages"     # (U-OV Sneltram)
    "/DITP/KV17cvlinfo",
    "/DITP/KV6posinfo",
    "/EBS/KV15messages",     # (EBS)
    "/EBS/KV17cvlinfo",
    "/EBS/KV6posinfo",
    "/GVB/KV15messages",     # (GVB)
    "/GVB/KV17cvlinfo",
    "/GVB/KV6posinfo",
    "/OPENOV/KV6posinfo"     # (De Lijn)
    "/OPENOV/KV15messages"   # (OpenEBS)
    "/OPENOV/KV17cvlinfo",
    "/QBUZZ/KV15messages",   # (QBuzz)
    "/QBUZZ/KV17cvlinfo",
    "/QBUZZ/KV6posinfo",
    "/RIG/KV15messages",     # (HTM, RET, Veolia)
    "/RIG/KV17cvlinfo",
    "/RIG/KV6posinfo",
    "/SYNTUS/KV6posinfo",    # (Syntus),
    "/SYNTUS/KV15message",
    "/SYNTUS/KV17cvlinfo"
]

for envelope in envelopes:
    socket.setsockopt_string(zmq.SUBSCRIBE, envelope)

CLIENTS = set()

async def websocket_handler(websocket, path):
    CLIENTS.add(websocket)
    print("Added client")
    try:
        async for _ in websocket:
            pass
    finally:
        print("Removing client")
        CLIENTS.remove(websocket)

async def main():
    host = "localhost"
    port = 9160

    async with serve(websocket_handler, host, port):
        print(f"Websocket server started at ws://{host}:{port}")
        
        while True:
            message = socket.recv()
            assert type(message) == bytes

            for websocket in CLIENTS.copy():
                try:
                    await websocket.send(message)
                except websockets.ConnectionClosed:
                    pass
            await asyncio.sleep(0.01)

asyncio.run(main())
