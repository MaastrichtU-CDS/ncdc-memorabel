import json
import openshift as oc
import time

# The Pod "task_name" is invalid: metadata.name: Invalid value: "task_name": a DNS-1123 subdomain must 
# consist of lower case alphanumeric characters, '-' or '.', and must start and end with an alphanumeric 
# character (e.g. 'example.com', regex used for validation is 
# '[a-z0-9]([-a-z0-9]*[a-z0-9])?(\.[a-z0-9]([-a-z0-9]*[a-z0-9])?)*')]
task_name = "task-name"

#Exception OpenShiftPythonException
with open('template-start-up.json') as json_file:
    template = json.load(json_file)
    template["metadata"]["name"] = task_name
    template["metadata"]["labels"]["task"] = task_name
    output = oc.create(template)
    #print(output)
    #if output['status'] != 0:
    #    print("Error")

c = oc.selector('pods', labels={"task": task_name, "app": "start-up-app"})
obj = c.objects()

# Error in 'actions' (list) - 'err' for status 'status'
if len(obj) == 0:
    raise Exception("Error")
elif len(obj) > 1:
    raise Exception("Error")
print("Created succesfully")
container_info = obj[0].as_dict()

n_tries = 0
complete = False
while n_tries < 20 and not complete:
    time.sleep(10)
    print("Another try")
    obj[0].refresh()
    if 'status' in container_info and 'phase' in container_info['status'] and container_info['status']['phase'] == 'Succeeded':
        complete = True
    n_tries += 1

#Validate the exit code shouyld be
#container_info['status']['containerStatuses'][0]['state']['terminated']['exitCode']
# if container_info['status']['exitCode'] != 0:
#     raise Exception("test")

c.delete()