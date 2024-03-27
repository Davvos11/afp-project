
# Table 'actual_arrivals'

- timestamp: TEXT
- stop_code: TEXT
- punctuality: INTEGER
- journey_id: INTEGER
- lineplanningnumber: TEXT
- type: TEXT
    ('DEPARTURE' or 'ARRIVAL')

# Table 'stops'

- stop_code: TEXT
- name: TEXT
- gps_lat: REAL
- gps_lon: REAL



# Table 'journeys' (not yet implemented)

- journey_id: INTEGER
- vehicle_id: INTEGER
- operator: VARCHAR
