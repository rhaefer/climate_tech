from dataclasses import dataclass
from enum import Enum
import calendar
import math
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pvlib
import pytz
from pathlib import Path
from typing import NamedTuple

from openmeteo_py.Hourly import Hourly
from openmeteo_py.Daily import Daily
from openmeteo_py.Options import Options
from openmeteo_py.OWmanager import OWmanager

# Latitude, Longitude for Rabat,Morocco
latitude = 38.918158
longitude = -119.981612

hourly = Hourly()
daily = Daily()
options = Options(latitude,longitude)
mgr = OWmanager(options,
    hourly.all(),
    daily.all())
# Download data
meteo = mgr.get_data()
meteo = pd.DataFrame(meteo)
# !pip install pvlib --quiet

import datetime
import json
import requests
# 3rd party packages
from IPython import display
import pandas as pd

#### energy generation #####
default_end_date = datetime.date.today().isoformat()
default_start_date = (datetime.date.today() - datetime.timedelta(days=365)).isoformat()

def get_eia_timeseries(
    url_segment,
    facets,
    value_column_name="value",
    start_date=default_start_date,
    end_date=default_end_date,
):
    """
    A generalized helper function to fetch data from the EIA API
    """

    api_url = f"https://api.eia.gov/v2/electricity/rto/{url_segment}/data/?api_key=KRgt1VeaP7ThrB5gfi4QXIXSVLwoyzWLQgtPxO3B"

    response_content = requests.get(
        api_url,
        headers={
            "X-Params": json.dumps(
                {
                    "frequency": "daily",
                    "data": ["value"],
                    "facets": dict(**{"timezone": ["Pacific"]}, **facets),
                    "start": start_date,
                    "end": end_date,
                    "sort": [{"column": "period", "direction": "desc"}],
                    "offset": 0,
                    "length": 5000,  # This is the maximum allowed
                }
            )
        },
    ).json()

    # Sometimes EIA API responses are nested under a "response" key. Sometimes not 🤷
    if "response" in response_content:
        response_content = response_content["response"]

    # Handle warnings by displaying them to the user
    if "warnings" in response_content:
        display.display(
            display.Markdown(f"Warning(s) returned from EIA API:"),
            response_content["warnings"],
        )
    print(f"{len(response_content['data'])} rows returned")

    # Convert the data to a Pandas DataFrame and clean it up for plotting
    dataframe = pd.DataFrame(response_content["data"])
    dataframe["timestamp"] = dataframe["period"].apply(
        pd.to_datetime, format='%Y-%m-%d'
    )
    dataframe.rename(columns={"value": value_column_name}, inplace=True)
    return dataframe


def get_eia_grid_mix_timeseries(balancing_authorities, **kwargs):
    """
    Fetch electricity generation data by fuel type
    """
    return get_eia_timeseries(
        url_segment="daily-fuel-type-data",
        facets={"respondent": balancing_authorities},
        value_column_name="Generation (MWh)",
        **kwargs,
    )


def get_eia_net_demand_and_generation_timeseries(balancing_authorities, **kwargs):
    """
    Fetch electricity demand data
    """
    return get_eia_timeseries(
        url_segment="daily-region-data",
        facets={
            "respondent": balancing_authorities,
            "type": ["D", "NG", "TI"],  # Filter out the "Demand forecast" (DF) type
        },
        value_column_name="Demand (MWh)",
        **kwargs,
    )


def get_eia_interchange_timeseries(balancing_authorities, **kwargs):
    """
    Fetch electricity interchange data (imports & exports from other utilities)
    """
    return get_eia_timeseries(
        url_segment="daily-interchange-data",
        facets={"toba": balancing_authorities},
        value_column_name=f"Interchange to local BA (MWh)",
        **kwargs,
    )
    
LOCAL_BALANCING_AUTHORITY = "BANC"

local_generation_grid_mix = get_eia_grid_mix_timeseries(
    [LOCAL_BALANCING_AUTHORITY],
    # Optional: uncomment the lines below to try looking at a different time range to get data from other seasons.
    # start_date="2022-01-01",
    # end_date="2023-01-01",
)



demand_df = get_eia_net_demand_and_generation_timeseries([LOCAL_BALANCING_AUTHORITY])
interchange_df = get_eia_interchange_timeseries([LOCAL_BALANCING_AUTHORITY])
interchange_df

interchange_df['Interchange to local BA (MWh)'] = pd.to_numeric(interchange_df['Interchange to local BA (MWh)'], errors='coerce')

# How much energy is both generated and consumed locally
def get_energy_generated_and_consumed_locally(df):
    demand_stats = df.groupby("type-name")["Demand (MWh)"].sum()
    # If local demand is smaller than net (local) generation, that means: amount generated and used locally == Demand (net export)
    # If local generation is smaller than local demand, that means: amount generated and used locally == Net generation (net import)
    # Therefore, the amount generated and used locally is the minimum of these two
    return min(demand_stats["Demand"], demand_stats["Net generation"])


energy_generated_and_used_locally = demand_df.groupby("timestamp").apply(
    get_energy_generated_and_consumed_locally
)

consumed_locally_column_name = "Power consumed locally (MWh)"

# How much energy is imported and then used locally, grouped by the source BA (i.e. the BA which generated the energy)
energy_imported_then_consumed_locally_by_source_ba = (
    interchange_df.groupby(["timestamp", "fromba"])[
        "Interchange to local BA (MWh)"
    ].sum()
    # We're only interested in data points where energy is coming *in* to the local BA, i.e. where net export is negative
    # Therefore, ignore positive net exports
    .apply(lambda interchange: max(interchange, 0))
)

# Combine these two together to get all energy used locally, grouped by the source BA (both local and connected)
energy_consumed_locally_by_source_ba = pd.concat(
    [
        energy_imported_then_consumed_locally_by_source_ba.rename(
            consumed_locally_column_name
        ).reset_index("fromba"),
        pd.DataFrame(
            {
                "fromba": LOCAL_BALANCING_AUTHORITY,
                consumed_locally_column_name: energy_generated_and_used_locally,
            }
        ),
    ]
).reset_index()

# First, get a list of all source BAs: our local BA plus the ones we're importing from
all_source_bas = energy_consumed_locally_by_source_ba["fromba"].unique().tolist()

# Then, fetch the fuel type breakdowns for each of those BAs
generation_types_by_ba = get_eia_grid_mix_timeseries(all_source_bas).rename(
    {"respondent": "fromba", "type-name": "generation_type"}, axis="columns"
)

all_source_bas

total_generation_by_source_ba = generation_types_by_ba.groupby(["timestamp", "fromba"])[
    "Generation (MWh)"
].sum()

generation_types_by_ba_with_totals = generation_types_by_ba.join(
    total_generation_by_source_ba,
    how="left",
    on=["timestamp", "fromba"],
    rsuffix=" Total",
)
generation_types_by_ba_with_totals['Generation (MWh)'] = pd.to_numeric(
    generation_types_by_ba_with_totals['Generation (MWh)'], errors='coerce'
)

generation_types_by_ba_with_totals['Generation (MWh) Total'] = pd.to_numeric(
    generation_types_by_ba_with_totals['Generation (MWh) Total'], errors='coerce'
)

generation_types_by_ba_with_totals.fillna(0, inplace=True)

generation_types_by_ba_with_totals["Generation (% of BA generation)"] = (
    generation_types_by_ba_with_totals["Generation (MWh)"]
    / generation_types_by_ba_with_totals["Generation (MWh) Total"]
)
generation_types_by_ba_with_totals_and_source_ba_breakdown = generation_types_by_ba_with_totals.merge(
    energy_consumed_locally_by_source_ba.rename(
        {"Power consumed locally (MWh)": "Power consumed locally from source BA (MWh)"},
        axis="columns",
    ),
    on=["timestamp", "fromba"],
)
full_df_reindexed = (
    generation_types_by_ba_with_totals_and_source_ba_breakdown.set_index(
        ["timestamp", "fromba", "generation_type"]
    )
)
full_df_reindexed['Power consumed locally from source BA (MWh)'] = pd.to_numeric(
    full_df_reindexed['Power consumed locally from source BA (MWh)'], errors='coerce'
)

full_df_reindexed['Generation (% of BA generation)'] = pd.to_numeric(
    full_df_reindexed['Generation (% of BA generation)'], errors='coerce'
)

usage_by_ba_and_generation_type = (
    (
        full_df_reindexed["Power consumed locally from source BA (MWh)"]
        * full_df_reindexed["Generation (% of BA generation)"]
    )
    .rename("Usage (MWh)")
    .reset_index()
)
usage_by_ba_and_generation_type

#### home energy model ####
# Also define a few permanent constants
JOULES_PER_KWH = 3.6e+6
JOULES_PER_MEGAJOULE = 1e6
SECONDS_PER_HOUR = 3600
AIR_VOLUMETRIC_HEAT_CAPACITY = 1200 # Energy in joules per cubic meter of air per degree K. (J/m3/K)

# To keep things tidier, we define a HomeCharacteristics dataclass to bunch all the defined and calculated attributes together
@dataclass
class HomeCharacteristics:
    latitude: float
    longitude: float
    heating_setpoint_c: int
    cooling_setpoint_c: int
    hvac_capacity_w: int
    hvac_overall_system_efficiency: int
    conditioned_floor_area_sq_m: int
    ceiling_height_m: int
    wall_insulation_r_value_imperial: int
    ach50: int
    south_facing_window_size_sq_m: int
    window_solar_heat_gain_coefficient: int

    @property
    def building_volume_cu_m(self) -> int:
        return self.conditioned_floor_area_sq_m * self.ceiling_height_m

    @property
    def building_perimeter_m(self) -> float:
        # Assume the building is a 1-story square
        return math.sqrt(self.conditioned_floor_area_sq_m) * 4
    
    @property
    def surface_area_to_area_sq_m(self) -> float:
        # Surface area exposed to air = wall area + roof area (~= floor area, for 1-story building)
        return self.building_perimeter_m * self.ceiling_height_m + self.conditioned_floor_area_sq_m

    @property
    def ach_natural(self) -> float:
        # "Natural" air changes per hour can be roughly estimated from ACH50 with an "LBL_FACTOR"
        # https://building-performance.org/bpa-journal/ach50-achnat/
        LBL_FACTOR = 17
        return self.ach50 / LBL_FACTOR

    @property
    def wall_insulation_r_value_si(self) -> float:
        # The R-values you typically see on products in the US will be in imperial units (ft^2 °F/Btu)
        # But our calculations need SI units (m^2 °K/W)
        return self.wall_insulation_r_value_imperial / 5.67 # SI units: m^2 °K/W

    @property
    def building_heat_capacity(self) -> int:
        # Building heat capacity
        # How much energy (in kJ) do you have to put into the building to change the indoor temperature by 1 degree?
        # Heat capacity unit: Joules per Kelvin degree (kJ/K)
        # A proper treatment of these factors would include multiple thermal mass components,
        # because the walls, air, furniture, foundation, etc. all store heat differently.
        # More info: https://www.greenspec.co.uk/building-design/thermal-mass/
        HEAT_CAPACITY_FUDGE_FACTOR = 1e5
        return self.building_volume_cu_m * HEAT_CAPACITY_FUDGE_FACTOR

home = HomeCharacteristics(
    ## Location
    # Change the two lines below to your home's latitude and longitude. (Find on Google Maps: https://support.google.com/maps/answer/18539)
    latitude = 38.918158, # South Lake Tahoe, CA
    longitude = -119.981612, # South Lake Tahoe, CA

    ## HVAC temperature setpoints (i.e. your thermostat settings)
    # Your HVAC system will start heating your home if the indoor temperature is below HEATING_SETPOINT_C (house is too cold)
    # It will start cooling your home if the indoor temperature is above COOLING_SETPOINT_C (house is too warm)
    # Change the two lines below to match your thermostat settings
    heating_setpoint_c=20, # ~65f
    cooling_setpoint_c=27, # ~80f

    ## HVAC system characteristics
    hvac_capacity_w=10000,
    # Different types of HVAC systems have different efficiencies (note: this is a hand-wavy approximation):
    #  - Old boiler with uninsulated pipes = ~0.5
    #  - Electric radiator = ~1
    #  - High-efficiency heat pump = ~4 (how can this be higher than 1?? heat pumpts are magical..) 
    hvac_overall_system_efficiency=1,

    ## Home dimensions
    # Note: these are in SI units. If you're used to Imperial: one square meter is 10.7639 sq. ft
    conditioned_floor_area_sq_m=130, # ~1400sqft
    ceiling_height_m=2.4, # 8ft ceilings (pretty tall)

    ## Wall Insulation
    # R value (SI): temperature difference (K) required to create 1 W/m2 of heat flux through a surface. Higher = better insulated
    wall_insulation_r_value_imperial=15, # Imperial units: ft^2 °F/Btu

    ## Air changes per hour at 50 pascals.
    # This is a measure of the "leakiness" of the home: 3 is pretty tight, A "passive house" is < 0.6
    # This number is measured in a "blower door test", which pressurizes the home to 50 pascals
    ach50=10,

    ## Window area
    # We're only modeling South-facing windows, as they have the largest effect from solar irradiance (in the Northern hemisphere)
    # We're assuming the window has an R value matching the walls (so we don't have to model it separately)
    # Change the line below to roughly match the size of your south-facing windows
    south_facing_window_size_sq_m=10, # ~110 sq ft
    # Solar Heat Gain Coefficient (SHGC) is a ratio of how much of the sun's energy makes it through the window (0-1)
    # Different types of windows have different values, e.g. a Double-pane, Low-E, H-Gain window SHGC=0.56
    window_solar_heat_gain_coefficient=0.5,
)

# In this cell, we're using `pvlib` to fetch historical "solar weather" data for our chosen location for a specific year in the past
# "Solar weather" is how much sun we got at this location

# We're going to simulate our home's electricity usage over a year, using historical weather data for an actual year in the past
# 2022 is the most recent year for which we can get historical solar weather data from NREL (but you could choose an earlier year)
SIMULATION_YEAR = 2022

solar_weather_timeseries, solar_weather_metadata = pvlib.iotools.get_psm3(
    latitude=home.latitude,
    longitude=home.longitude,
    names=SIMULATION_YEAR,
    api_key="Rly0vof5nkAUsp9oOcbDBjzhRkG4u66oDoT7wpFG",
    email='reidhaefer14@gmail.com',
    map_variables=True,
    leap_day=True,
)
home_latitude = home.latitude
home_longitude = home.longitude

solar_position_timeseries = pvlib.solarposition.get_solarposition(
    time=solar_weather_timeseries.index,
    latitude=home.latitude,
    longitude=home.longitude,
    altitude=100, # Assume close to sea level, this doesn't matter much
    temperature=solar_weather_timeseries["temp_air"],
)

window_irradiance = pvlib.irradiance.get_total_irradiance(
    90, # Window tilt (90 = vertical)
    180, # Window compass orientation (180 = south-facing)
    solar_position_timeseries.apparent_zenith,
    solar_position_timeseries.azimuth,
    solar_weather_timeseries.dni,
    solar_weather_timeseries.ghi,
    solar_weather_timeseries.dhi,
)

# In this cell, we put it all together and simulate the electricity usage of our HVAC system, given a year of historical weather
# You do not need to make changes to this cell

# We're modeling the effect of three external sources of energy that can affect the temperature of the home: 
#  1. Conductive heat gain or loss through contact with the walls and roof (we ignore the floor), given outdoor temperature
#  2. Air change heat gain or loss through air changes between air in the house and outside, given outdoor temperature
#  3. Radiant heat gain from sun coming in south-facing windows

# We then model our HVAC system as heating/cooling/off depending on whether the temperature is above or below desired setpoints

def calculate_next_timestep(
    timestamp,
    indoor_temperature_c,
    outdoor_temperature_c,
    irradiance,
    home: HomeCharacteristics,
    dt=pd.Timedelta(minutes=10) # Defaulting to a timestep of 10 minute increments
):
    '''
    This function calculates the ΔT (the change in indoor temperature) during a single timestep given:
      1. Previous indoor temperature
      2. Current outdoor temperature (from historical weather data)
      3. Current solar irradiance through south-facing windows (from historical weather data)
      4. Home and HVAC characteristics
    '''

    temperature_difference_c = outdoor_temperature_c - indoor_temperature_c

    # Calculate energy in to building

    # 1. Energy conducted through walls & roof (in Joules, J)
    # Conduction
    # Q = U.A.dT, where U = 1/R
    # Convection:
    # Q = m_dot . Cp * dT <=> Q = V_dot * Cv * dT (Cv = Rho * Cp)

    power_in_through_surface_w = (
        temperature_difference_c * home.surface_area_to_area_sq_m / home.wall_insulation_r_value_si
    )
    energy_from_conduction_j = power_in_through_surface_w * dt.seconds

    # 2. Energy exchanged through air changes with the outside air (in Joules, J)
    air_change_volume = (
        dt.seconds * home.building_volume_cu_m * home.ach_natural / SECONDS_PER_HOUR
    )
    energy_from_air_change_j = (
        temperature_difference_c * air_change_volume * AIR_VOLUMETRIC_HEAT_CAPACITY
    )

    # 3. Energy radiating from the sun in through south-facing windows (in Joules, J)
    energy_from_sun_j = (
        home.south_facing_window_size_sq_m
        * home.window_solar_heat_gain_coefficient
        * irradiance
        * dt.seconds
    )

    # 4. Energy added or removed by the HVAC system (in Joules, J)
    # HVAC systems are either "on" or "off", so the energy they add or remove at any one time equals their total capacity
    if indoor_temperature_c < home.heating_setpoint_c:
        hvac_mode = "heating"
        energy_from_hvac_j = home.hvac_capacity_w * dt.seconds
    elif indoor_temperature_c > home.cooling_setpoint_c:
        hvac_mode = "cooling"
        energy_from_hvac_j = -home.hvac_capacity_w * dt.seconds
    else:
        hvac_mode = "off"
        energy_from_hvac_j = 0

    total_energy_in_j = (
        energy_from_conduction_j
        + energy_from_air_change_j
        + energy_from_sun_j
        + energy_from_hvac_j
    )

    # ΔT is the change in indoor temperature during this timestep resulting from the total energy input
    delta_t = total_energy_in_j / home.building_heat_capacity

    return pd.Series(
        {
            "timestamp": timestamp,
            "temperature_difference_c": temperature_difference_c,
            "Conductive energy (J)": energy_from_conduction_j,
            "Air change energy (J)": energy_from_air_change_j,
            "Radiant energy (J)": energy_from_sun_j,
            "HVAC energy (J)": energy_from_hvac_j,
            "hvac_mode": hvac_mode,
            "Net energy xfer": total_energy_in_j,
            "ΔT": delta_t,
            "Outdoor Temperature (C)": outdoor_temperature_c,
            "Indoor Temperature (C)": indoor_temperature_c + delta_t,
            # Actual energy consumption from the HVAC system:
            "HVAC energy use (kWh)": abs(energy_from_hvac_j) / (JOULES_PER_KWH * home.hvac_overall_system_efficiency)
        }
    )

# Since we're starting in January, let's assume our starting temperature is the heating setpoint
previous_indoor_temperature_c = home.heating_setpoint_c

timesteps = []
for timestamp in solar_weather_timeseries.index:
    new_timestep = calculate_next_timestep(
        timestamp=timestamp,
        indoor_temperature_c=previous_indoor_temperature_c,
        outdoor_temperature_c=solar_weather_timeseries.loc[timestamp].temp_air,
        irradiance=window_irradiance.loc[timestamp].poa_direct,
        home=home,
    )
    timesteps.append(new_timestep)
    previous_indoor_temperature_c = new_timestep["Indoor Temperature (C)"]


baby_energy_model = pd.DataFrame(timesteps)
baby_energy_model

# For each month, let's look at the overall energy balance:
# Where is the thermal energy in the house coming from, and where is it going to?
energy_transfer_columns = [col for col in baby_energy_model.columns if col.endswith("(J)")]
get_month=lambda idx: baby_energy_model.loc[idx]['timestamp'].month
monthly_energy_balance_mj = baby_energy_model.groupby(by=get_month)[energy_transfer_columns].sum() / JOULES_PER_MEGAJOULE

monthly_energy_balance_mj['month'] = monthly_energy_balance_mj.index.map(lambda month_idx: f'{month_idx:0=2} - {calendar.month_name[month_idx]}')

monthly_energy_balance_tidy = monthly_energy_balance_mj.melt(id_vars='month')

## export to csv
energy_consumed_locally_by_source_ba.to_csv("data/energy_consumed_locally_by_source_ba.csv")

monthly_energy_balance_tidy.to_csv("data/monthly_energy_balance_tidy.csv")

usage_by_ba_and_generation_type.to_csv("data/usage_by_ba_and_generation_type.csv")

local_generation_grid_mix.to_csv("data/local_generation_grid_mix.csv")

baby_energy_model.to_csv("data/baby_energy_model.csv")

meteo.to_csv("data/meteo.csv")
