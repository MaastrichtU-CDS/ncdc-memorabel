import os
import subprocess
import time
import multiprocessing as mp
import logging

JOBS = os.getenv('JOBS') or 1
INPUT_FOLDER = os.getenv('INPUT')
OUTPUT_FOLDER = os.getenv('SAVE_PATH')
LOG_PATH = os.getenv('LOG_PATH')

# mp.cpu_count()

def run_command(command, success_message, error_message):
    """ Runs a bash command """
    process = subprocess.run(command, capture_output=True, check=False)
    if process.returncode == 0:
        logging.info(success_message)
    else:
        logging.error(error_message)
        logging.error(process.stderr.decode("utf-8"))

def pre_process(scan):
    """ Apply the pre-processing script.
    """
    logging.info(f'Pre-processing scan {scan}')
    run_command(
        ['./pre_processing.sh', '-i', f'{INPUT_FOLDER}/{scan}/aseg.mgz', '-n', f'{scan}'],
        f'Successfully pre-processed scan {scan}',
        f'Error while processing scan {scan}'
    )

if __name__ == '__main__':
    logging.basicConfig(filename=f'{LOG_PATH}/pre-processing.log', level=logging.DEBUG)
    logging.info('Starting the process')
    with mp.Pool(processes=int(JOBS)) as pool:
        run = 1
        processing = []
        while run:
            logging.info('Checking if new scans are available')
            new_scans = [scan for scan in os.listdir(INPUT_FOLDER) if scan not in processing]
            if len(new_scans) > 0:
                logging.info(f'{len(new_scans)} scans available')
                for scan in new_scans:
                    if scan == "-1":
                        run = 0
                    else:
                        if not os.path.isfile(f"{OUTPUT_FOLDER}/GM_to_template_GM_med"):
                            processing.append(scan)
                            pool.apply_async(pre_process, args=(scan))
            else:
                logging.info("No scans found to process")
            time.sleep(30)
logging.info('Stopping the process')
