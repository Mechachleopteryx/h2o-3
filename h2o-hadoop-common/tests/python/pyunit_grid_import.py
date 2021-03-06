import sys
import os
sys.path.insert(1, os.path.join("..", "..", ".."))
import h2o
from tests import pyunit_utils
from h2o.grid.grid_search import H2OGridSearch
from h2o.estimators.gbm import H2OGradientBoostingEstimator


def grid_resume():
    train = h2o.import_file(path=pyunit_utils.locate("smalldata/iris/iris_wheader.csv"))

    hyper_parameters = {
        "learn_rate": [0.1, 0.01, .05],
        "ntrees": [10, 20]
    }
    export_dir = 'hdfs:///user/jenkins/grid_export_py'
    gs = H2OGridSearch(H2OGradientBoostingEstimator, hyper_params=hyper_parameters)
    gs.train(x=list(range(4)), y=4, training_frame=train)
    grid_id = gs.grid_id
    old_grid_model_count = len(gs.model_ids)
    print("Baseline grid has %d models" % old_grid_model_count)
    saved_path = h2o.save_grid(export_dir, grid_id)
    h2o.remove_all()

    train = h2o.import_file(path=pyunit_utils.locate("smalldata/iris/iris_wheader.csv"))
    grid = h2o.load_grid(saved_path)
    assert grid is not None
    assert len(grid.model_ids) == old_grid_model_count
    grid.train(x=list(range(4)), y=4, training_frame=train)
    print("Newly grained grid has %d models" % len(grid.model_ids))
    assert len(grid.model_ids) == old_grid_model_count


if __name__ == "__main__":
    pyunit_utils.standalone_test(grid_resume)
else:
    grid_resume()
