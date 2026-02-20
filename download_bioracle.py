import subprocess
import os

# 1. Configuration
output_dir = "/Volumes/share/Staff/mq41637526/Maina/habitat/parcels/cmip6/bio_oracle_results"
if not os.path.exists(output_dir):
    os.makedirs(output_dir)

# The base parameters from your links
scenario = "ssp585"
variable = "thetao_ltmax"
years = [2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090]

print(f"Starting automated wget downloads to: {output_dir}")

for year in years:
    # Construct the filename for this specific decade slice
    filename = f"{variable}_{scenario}_{year}.nc"
    filepath = os.path.join(output_dir, filename)
    
    # Construct the URL exactly as you formatted them
    # Note: We use the single year for both start and end to get that specific slice
    url = (
        f"https://erddap.bio-oracle.org/erddap/griddap/thetao_{scenario}_2020_2100_depthsurf.nc"
        f"?{variable}[({year}-01-01):1:({year}-01-01T00:00:00Z)]"
        f"[(-90.0):1:(90.0)][(-180.0):1:(180.0)]"
    )
    
    print(f"--- Downloading {year} slice ---")
    
    # Define the wget command
    # -O specifies the output filename
    # -q turns off the progress bar for a cleaner terminal (remove -q to see progress)
    wget_command = ["wget", "-O", filepath, url]
    
    try:
        subprocess.run(wget_command, check=True)
        print(f"Successfully saved to: {filename}")
    except subprocess.CalledProcessError as e:
        print(f"Failed to download {year}. Error: {e}")

print("\nAll downloads complete.")