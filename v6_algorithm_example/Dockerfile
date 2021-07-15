# This specifies our base image. This base image contains some commonly used
# dependancies and an install from all vantage6 packages. You can specify a
# different image here (e.g. python:3). In that case it is important that
# `vantage6-client` is a dependancy of you project as this contains the wrapper
# we are using in this example.
FROM harbor.vantage6.ai/algorithms/algorithm-base

# Change this to the package name of your project. This needs to be the same
# as what you specified for the name in the `setup.py`.
ARG PKG_NAME="algorithm_test_cluster"

RUN apt-get update
RUN apt-get install -y apt-utils gcc libpq-dev wget iputils-ping

# This will install your algorithm into this image.
COPY . /app
RUN pip install --no-cache-dir /app

# This will run your algorithm when the Docker container is started. The
# wrapper takes care of the IO handling (communication between node and
# algorithm). You dont need to change anything here.
ENV PKG_NAME=${PKG_NAME}
CMD python -c "from algorithm_test_cluster.docker_wrapper import docker_wrapper; docker_wrapper('${PKG_NAME}')"
