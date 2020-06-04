FROM python:3.7

WORKDIR /app

RUN touch input.txt
RUN touch output.txt

COPY requirements.txt /app
COPY run.py /app

RUN pip install --upgrade pip
RUN pip install -r requirements.txt

CMD ["python", "run.py"]