import sqlite3
from pathlib import Path
import csv
import xml.etree.ElementTree as ET

# This files generated the initial database, database-subscriber.db
# It contains a table 'stops', which contains information of stops, like their
# name (the 'actual_arrivals' database only contains an id 'stop_code').
# This stops table is also used for the filtering: only the stops within the
# geographic region of interest are included. Subscriber.hs only logs messages
# for stops present in this table.

# I didn't get around to doing this in Haskell ;(

# The stops.txt can be downloaded from http://gtfs.ovapi.nl/gtfs-nl.zip
stop_src = Path.cwd() / "stops.txts"

# This file will be created
db_file = Path.cwd() / "database-subscriber.db"

# This file is optional. It is only necessary when the `lines' table or other
# info needs to be added as well.
#
# Files for QBUZZ U-OV can be download from
# http://data.ndovloket.nl/netex/qbuzz/
# Downloads for other providers: http://data.ndovloket.nl/netex/
netex_file = Path.cwd() / "NeTEx_QBUZZ_U-OV_20240322_20240328.xml"


def add_stops(con: sqlite3.Connection):
    con.execute(
        """
        DROP TABLE IF EXISTS stops
        """
    )

    con.execute(
        """
        CREATE TABLE IF NOT EXISTS stops (
            stop_code TEXT PRIMARY KEY,
            name TEXT,
            gps_lat REAL,
            gps_lon REAL
        )
        """
    )
    empty_count = 0
    valid_count = 0

    with stop_src.open() as f_raw:
        f = csv.reader(f_raw)
        header = next(f)

        for row in f:
            stop_code = row[header.index("stop_code")]
            stop_name = row[header.index("stop_name")]
            gps_lat = float(row[header.index("stop_lat")])
            gps_lon = float(row[header.index("stop_lon")])

            if gps_lat < 51.9467 or gps_lat > 52.189383:
                continue
            if gps_lon < 4.923786 or gps_lon > 5.30204:
                continue

            if len(stop_code) == 0:
                print(f"stop_code is empty for stop_name {stop_name}")
                empty_count += 1
                continue

            valid_count += 1

            # 52.1465783, 4.9867164

            # 52.189383, 4.923786
            # 51.9467, 5.30204

            print(f"{stop_code:>10} | {stop_name}")

            con.execute(
                """
                INSERT OR REPLACE INTO stops (stop_code, name, gps_lat, gps_lon)
                VALUES (?, ?, ?, ?)
                """,
                (stop_code, stop_name, gps_lat, gps_lon),
            )

            # print(row)
    print(f"empty_count: {empty_count}")
    print(f"valid_count: {valid_count}")


# def _get_service_frames():
#     root = ET.fromstring(netex_file.read_text())
#     for dataObject in root.iter("{http://www.netex.org.uk/netex}dataObjects"):
#         for compositeFrame in dataObject:
#             # print(compositeFrame.attrib)
#             frame_id = compositeFrame.attrib["id"]
#             [frame_type] = list(compositeFrame.iter("{http://www.netex.org.uk/netex}TypeOfFrameRef"))

#             if frame_type.attrib["ref"] != "BISON:TypeOfFrame:NL_TT_BASELINE":
#                 continue

#             print(frame_id)
#             [frames] = list(compositeFrame.iter("{http://www.netex.org.uk/netex}frames"))
#             for frame in frames.iter():
#                 [service_frame] = list(frame.iter("{http://www.netex.org.uk/netex}ServiceFrame"))
#                 yield service_frame


def get_service_frame():
    # [service_frame] = list(_get_service_frames())
    # return service_frame
    root = ET.fromstring(netex_file.read_text())

    [service_frame] = list(root.iter("{http://www.netex.org.uk/netex}ServiceFrame"))
    return service_frame

def get_timetable_frame():
    root = ET.fromstring(netex_file.read_text())

    [frame] = list(root.iter("{http://www.netex.org.uk/netex}TimetableFrame"))
    return frame



def add_lines(con: sqlite3.Connection):
    # root = ET.fromstring(netex_file.read_text())
    # for dataObject in root.iter("{http://www.netex.org.uk/netex}dataObjects"):
    #     for compositeFrame in dataObject:
    #         # print(compositeFrame.attrib)
    #         frame_id = compositeFrame.attrib["id"]
    #         frame_type = next(compositeFrame.iter("{http://www.netex.org.uk/netex}TypeOfFrameRef"))

    #         if frame_type.attrib["ref"] != "BISON:TypeOfFrame:NL_TT_BASELINE":
    #             continue

    #         print(frame_id)
    #         frames = next(compositeFrame.iter("{http://www.netex.org.uk/netex}frames"))
    #         for frame in frames.iter()
    con.execute(
        """
        CREATE TABLE IF NOT EXISTS lines (
            lineplanningnumber TEXT PRIMARY KEY,
            name TEXT,
            public_code TEXT,
            color_bg TEXT,
            color_fg TEXT,
            branding_ref TEXT,
            transport_mode TEXT
        )
        """
    )

    service_frame = get_service_frame()

    [lines] = service_frame.findall("{http://www.netex.org.uk/netex}lines")
    print(lines)
    for line in lines.findall("{http://www.netex.org.uk/netex}Line"):
        [privateCode] = line.findall("{http://www.netex.org.uk/netex}PrivateCode")
        assert privateCode.attrib["type"] == "LinePlanningNumber"
        lineplanningnumber = privateCode.text
        assert lineplanningnumber is not None
        lineplanningnumber = lineplanningnumber.strip()
        assert len(lineplanningnumber) > 0

        name = line.find("{http://www.netex.org.uk/netex}Name").text.strip()
        color_bg = (
            line.find("{http://www.netex.org.uk/netex}Presentation")
            .find("{http://www.netex.org.uk/netex}Colour")
            .text
        )
        color_fg = (
            line.find("{http://www.netex.org.uk/netex}Presentation")
            .find("{http://www.netex.org.uk/netex}TextColour")
            .text
        )
        branding_ref = line.find("{http://www.netex.org.uk/netex}BrandingRef").attrib[
            "ref"
        ]
        transport_mode = line.find("{http://www.netex.org.uk/netex}TransportMode").text
        assert transport_mode in ("bus", "tram")
        public_code = line.find("{http://www.netex.org.uk/netex}PublicCode").text

        print(f"{lineplanningnumber:>10} | {public_code} | {name}")

        con.execute(
            """
            INSERT OR REPLACE INTO lines (
                lineplanningnumber, name, public_code,
                color_bg, color_fg, branding_ref, transport_mode
            )
            VALUES (?, ?, ?, ?, ?, ?, ?)
            """,
            (
                lineplanningnumber,
                name,
                public_code,
                color_bg,
                color_fg,
                branding_ref,
                transport_mode,
            ),
        )

        # print(dataObject)

    # print(root)

def add_journeys_schedule(con: sqlite3.Connection):
    timetable_frame = get_timetable_frame()
    [vehicle_journeys] = timetable_frame.findall("{http://www.netex.org.uk/netex}verhicleJourneys")
    for vehicle_journey in vehicle_journeys.findall("{http://www.netex.org.uk/netex}ServiceJourney"):
        print(vehicle_journey)


with sqlite3.connect(db_file) as con:
    add_stops(con)
    # add_lines(con)
    # add_journeys_schedule(con)
    con.commit()
