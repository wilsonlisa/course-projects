import json
import pyspark
import pprint
# from numpy import mean
from operator import add
# from pyspark.sql.functions import avg
# from pyspark.sql.functions import stddev
# from numpy import stdev
import math

def StdDev( data ):
    suma = 0
    for x in data: suma += ((x - sum(data)/len(data))**2)  # every iteration subtracks the mean value of a list [sum(data)/len(data)]  from every [x] element of a list and then raises to the second power [**2]
    print((suma/(len(data)-1))**0.5) # then divide by n-1 [len(data)-1] and extract the root of 2 [**0.5]

sc = pyspark.SparkContext()

bucket = sc._jsc.hadoopConfiguration().get('fs.gs.system.bucket')
project = sc._jsc.hadoopConfiguration().get('fs.gs.project.id')
input_directory = 'gs://{}/hadoop/tmp/bigquery/pyspark_input'.format(bucket)
output_directory = 'gs://{}/pyspark_demo_output3'.format(bucket)

conf = {
    # Input Parameters.
    'mapred.bq.project.id': project,
    'mapred.bq.gcs.bucket': bucket,
    'mapred.bq.temp.gcs.path': input_directory,
    'mapred.bq.input.project.id': 'final-project-269604',
    'mapred.bq.input.dataset.id': 'taxi',
    'mapred.bq.input.table.id': 'chrysler',
}

table_data = sc.newAPIHadoopRDD(
    'com.google.cloud.hadoop.io.bigquery.JsonTextBigQueryInputFormat',
    'org.apache.hadoop.io.LongWritable',
    'com.google.gson.JsonObject',
    conf=conf
)

vals = table_data.values()
vals = vals.map(lambda line: json.loads(line))
vals_wd = vals.map(lambda x: (str(x['weekday_pickup']), float(x['trip_distance'])))
sum_wd = vals_wd.reduceByKey(add)
count_wd = vals_wd.countByKey()
avg_wd = sum_wd.map(lambda x: (x[0], float(x[1]/count_wd[x[0]])))

# hour_avg2 = vals_hour.groupByKey().mean()
# hour_std = vals_hour.reduceByKey(stdev)
# hour_std2 = vals_hour.groupByKey().map(lambda x: (x[0], stddev(x[1])))
# hour_std = vals_hour.groupByKey().stdev()

# standard deviation
# had trouble using reduceByKey with built-in standard deviation functions in various packages
# so used code from the following blog post
# https://blog.scottlogic.com/2016/12/19/spark-unaffordable-britain.html
countByKey = vals_wd.map(lambda kvp: (kvp[0], 1)).reduceByKey(lambda a,b: a + b)
totalByKey = vals_wd.reduceByKey(lambda a,b: a + b)
sumSqByKey = vals_wd.map(lambda kvp: (kvp[0], kvp[1]**2)).reduceByKey(lambda a,b: a + b)
mean = totalByKey.join(countByKey).map(lambda kvp: (kvp[0], kvp[1][0] / kvp[1][1]))
avgSquare = sumSqByKey.join(countByKey).map(lambda kvp: (kvp[0], kvp[1][0] / kvp[1][1]))
std_wd = avgSquare.join(mean).map(lambda kvp: (kvp[0], math.sqrt(kvp[1][0] - kvp[1][1]**2)))

# pprint.pprint(vals_crd.collect())
pprint.pprint(mean.collect())

pprint.pprint(avg_wd.collect())
pprint.pprint(std_wd.collect())

wd_avgstd = avg_wd.union(std_wd)
pprint.pprint(wd_avgstd.collect())

wd_avgstd.saveAsTextFile(output_directory)

input_path = sc._jvm.org.apache.hadoop.fs.Path(input_directory)
input_path.getFileSystem(sc._jsc.hadoopConfiguration()).delete(input_path, True)
