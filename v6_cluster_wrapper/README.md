# Vantage6 Algorithm Wrapper to run in a Cluster

The NCDC Memorabel project requires more computational power to certain tasks that can't be provided by the default server running the vantage6 node. 
Being able to run an algorithm in a different machine while using the vantage6 infrastructure requires some adaptations.

:bangbang: The V6 node image required some changes in order to make the communication with the cluster possible. This algorithm wrapper assumes that the **vantage6 node is running the following image pmateus/vantage6-node-whitelisted:2.0.0** (find more documentation on this version in the following [repository](https://github.com/pedro-cmat/vantage6-node)).

## Solution

Each cohort will have a default docker image consisting of an adaptation from the usual algorithm wrapper used in vantage6.
This image will have the configurations required to deploy the algorithm in a different machine.
The algorithm can be generalize and only needs to have a standard way to input and output data, compliant with the wrapper.

## Maastricht POC

The `ncdc_maastricht_wrapper` provides a POC with an implementation for the algorithm wrapper.
This implementation makes use of the infrastructure available at the Maastricht node, a GPU cluster using Openshift.
The docker image is currently available under the following tag: `pmateus/ncdc-maastricht-wrapper`

The `docker_wrapper` function will communicate with the cluster using the `oc` CLI and the `openshift-client` python library.

## Testing

### Vantage6 Node

This algorithm wrapper assumes that the vantage6 node is running the following image `pmateus/vantage6-node-whitelisted:2.0.0` (find more documentation on this version in the following [repository](https://github.com/pedro-cmat/vantage6-node)).

The PHT infrastructure can be launched locally (allowing to test this approach more quickly) using one of the following demos:
- [Vantage6 Docker Demo](https://gitlab.com/UM-CDS/pht/vantage6-docker-demo)
- [Vantage6 RDB Demo](https://gitlab.com/UM-CDS/vantage6-rdb-demo)

### Algorithm example

An algorithm example to quickly test the process can be found in `v6_algorithm_example`.
The docker image is available under the following tag: `pmateus/test-cluster` (the method name would be `test_connection`)

### Creating a new task

Creating a task using the vantage6 client should follow this format:
```python
input_ = {
    "master": "true",
    "method":"master", 
    "args": ["algorithm-image", "method-name"],
    "kwargs": {
        # any data that needs to be provided
    }
}

print("Requesting to execute the task")
task = client.post_task(
    name="Task-name",
    image="gpu_image",
    collaboration_id=1,
    input_= input_,
    organization_ids=[1,2]
)
```

In the `args` section, two arguments should be provided:
- `algorithm-image`: The docker image with the algorithm that will run at the cluster (e.g. `pmateus/test-cluster`)
- `method-name`: The method name for the function in the algorithm (e.g. `test_connection`)

In the `pos_task` arguments:
- the `image` should be the placeholder that gets mapped to the local wrapper image in the node configurations

Example of the node configuration:
```yaml
application:
    api_key: 
    api_path:
    databases:
    ...
    docker_images_placeholders:
        gpu_image: pmateus/ncdc-maastricht-wrapper
```
