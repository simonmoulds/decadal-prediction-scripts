#!/usr/bin/env python3

import pandas as pd
# import click

from utils import _observed_field_preprocessor

# @click.command()
# @click.option('--inputdir', default='.', help='Input directory')
# @click.option('--outputdir', default='.', help='Output directory')
# @click.option('--stations', default='.', help='Stations')
# @click.option('--config', default='config.yml', help='YAML configuration file')
# def main(inputdir, outputdir, stations, config):

#     # Read station metadata and pull out info needed for preprocessor
#     station_metadata = pd.read_csv(stations)
#     station_ids = station_metadata['id'].to_list()
#     lat_coords = station_metadata['lat'].to_list()
#     lon_coords = station_metadata['lon'].to_list()
#     _observed_field_preprocessor(
#         inputdir, outputdir, station_ids,
#         lat_coords, lon_coords, config
#     )

# if __name__ == '__main__':
#     main()

# stations = snakemake.input['stations']
# # station_metadata = pd.read_csv(stations)
# station_metadata = pd.read_parquet(stations)
# station_ids = station_metadata['id'].to_list()
# lat_coords = station_metadata['lat'].to_list()
# lon_coords = station_metadata['lon'].to_list()
_observed_field_preprocessor(
    snakemake.config['input_data_root'],
    snakemake.params['outputdir'],
    snakemake.input['stations'],
    snakemake.input['grid'],
    # station_ids,
    # lat_coords,
    # lon_coords,
    snakemake.config
)
