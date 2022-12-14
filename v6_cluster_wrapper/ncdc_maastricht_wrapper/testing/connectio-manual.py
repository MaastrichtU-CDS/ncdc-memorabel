import json
import openshift as oc
import time
import uuid
import os
import subprocess

def run_command(command, success_message, error_message):
    """ Runs a bash command """
    process = subprocess.run(command, capture_output=True, check=False)
    if process.returncode == 0:
        print(success_message)
    else:
        print(error_message)
        print(process.stderr.decode("utf-8"))

def get_tasks(input_folder, output_folder, task_id):
    """ Get the tasks that should get executed in the cluster.
    """
    return [
        {
            "task": "start-up-app",
            "description": "Start up a new pod",
            "file": "template-start-up.json",
            "sleep": 20,
            "env": False,
            "task_command": [
                "sh",
                "-c",
                f"until [[ -f /mnt/data/data.json ]]; do echo waiting for the input; sleep 10; done;"
            ],
            "commands": [['oc', 'cp', '../data.json', f'{task_id}:/mnt/data/data.json']]
            #"commands": [['oc', 'cp', input_folder, f'{task_id}:/mnt/data/data.json']]
        },
        {
            "task": "test-container-app",
            "description": "Run the main algorithm",
            "file": "template-run-algorithm.json",
            "sleep": 30,
            "env": True,
        },
        {
            "task": "clear-up-app",
            "description": "Clear up and finish",
            "file": "template-clear-up.json",
            "sleep": 20,
            "env": False,
            "task_command": [
                "sh",
                "-c",
                f"until [[ ! -f /mnt/data/data.json ]]; do echo waiting to clear up; sleep 10; done;"
            ],
            "commands": [
                ['oc', 'cp', f'{task_id}:/mnt/data/output.json', './output.json'],
                #['oc', 'exec', task_id, 'rm', '-rf', '/mnt/data/data.json']
                ['kubectl', 'exec', task_id, '--', 'rm', '-rf', '/mnt/data/output.json'],
                ['kubectl', 'exec', task_id, '--', 'rm', '-rf', '/mnt/data/data.json']
            ]
        },
    ]

def run_task(task_id, task_definition):
    """ Run the task.
    """
    print(f"Creating new task {task_definition['task']} {task_id}")
    with open(task_definition['file']) as json_file:
        template = json.load(json_file)
        template["metadata"]["name"] = task_id
        template["metadata"]["labels"]["task"] = task_id
        if "task_command" in task_definition:
            template["spec"]["containers"][0]["command"] = task_definition["task_command"]
        output = oc.create(template)


    print("Checking pods available")
    c = oc.selector('pods', labels={"task": task_id, "app": task_definition["task"]})
    obj = c.objects()

    # Error in 'actions' (list) - 'err' for status 'status'
    if len(obj) == 0:
        raise Exception("Error")
    elif len(obj) > 1:
        raise Exception("Error")
    print("Created succesfully")
    container_info = obj[0].as_dict()

    #while container_info["status"]["phase"] != "Running" or container_info["status"]["phase"] != "Completed":
    while container_info["status"]["phase"] == "Pending":
        print("Waiting for the pod to be ready")
        time.sleep(10)
        obj[0].refresh()
        container_info = obj[0].as_dict()
        #print(container_info['status'])

    if "commands" in task_definition and task_definition["commands"]:
        for command in task_definition["commands"]:
            run_command(command, "Success", "Error")

    n_tries = 0
    complete = False
    while n_tries < 20 and not complete:
        if 'status' in container_info and 'phase' in container_info['status'] and container_info['status']['phase'] == 'Succeeded':
            complete = True
        time.sleep(task_definition["sleep"])
        print("Refreshing the pod status")
        obj[0].refresh()
        container_info = obj[0].as_dict()
        print(container_info['status']['phase'])
        n_tries += 1

    print(f"Successfully executed task {task_definition['task']}")
    #Validate the exit code shouyld be
    #container_info['status']['containerStatuses'][0]['state']['terminated']['exitCode']
    # if container_info['status']['exitCode'] != 0:
    #     raise Exception("test")

    c.delete()
    time.sleep(5)


task_id = str(uuid.uuid1())
tasks = get_tasks(
    "",
    "",
    task_id
)
for task in tasks:
    run_task(task_id, task)