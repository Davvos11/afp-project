
# Table 'actual_arrivals'

- timestamp: TEXT
- stop_code: TEXT
- punctuality: INTEGER
- journey_id: INTEGER
- lineplanningnumber: TEXT
- type: TEXT
    ('DEPARTURE' or 'ARRIVAL')
- dataownercode: TEXT
- vehicle_number: INTEGER


# Table 'stops'

- stop_code: TEXT
- name: TEXT
- gps_lat: REAL
- gps_lon: REAL
- stop_area_id: TEXT (maybe NULL) (not yet implemented)

# Table 'lines'

- lineplanningnumber: TEXT
- name: TEXT
- public_code: TEXT
- color_bg: TEXT
- color_fg: TEXT
- branding_ref: TEXT
- transport_mode: TEXT
    ('bus' or 'tram')


# Table 'stop_areas' (not yet implemented)

- id: TEXT
- name: TEXT


# Table 'availability_condition'

- id: TEXT
- from_date: TEXT (e.g. '2024-03-28T00:00:00Z')
- to_date: TEXT (e.g. '2024-04-12T00:00:00Z')
- validity_bits: TEXT (e.g. '1100011110011111')


# Table 'journeys_schedule'

- id: TEXT
- availability_condition_id: TEXT
- journey_id: INTEGER
- departure_time: TEXT (e.g. '06:00:00')
- journey_pattern: TEXT
- time_demand?


# Table 'journey_pattern'

- id: TEXT
- lineplanningnumber: TEXT

# Table 'journey_pattern_stops'

- journey_pattern_id: TEXT
- stop_code: TEXT



# Table 'journeys' (not yet implemented)

- journey_id: INTEGER
- vehicle_id: INTEGER
- operator: VARCHAR
