import os
import argparse
import sys

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--i", type=int, default=0, required=True)
    parser.add_argument("--job_file", type=str, default=None, required=True)
    args = parser.parse_args()

    with open(args.job_file, "r") as io:
        jobs = io.readlines()
    jobs = [j for j in jobs if j[0] != "#"]

    exit_code = os.system(jobs[args.i])  # run job
    sys.exit(exit_code)
    
    