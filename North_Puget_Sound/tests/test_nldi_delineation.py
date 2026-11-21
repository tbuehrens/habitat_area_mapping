"""Offline river-identity gates; no network calls or basin computations."""
import importlib.util
import math
from pathlib import Path
import shutil
import uuid

spec = importlib.util.spec_from_file_location("basins", Path("scripts/fetch_upstream_basins.py"))
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)

latitude = 48.5
origin = [-122.0, latitude]
degree = 1 / (111000 * math.cos(math.radians(latitude)))
samples = [[origin[0] + x * degree, latitude] for x in [-50, -33, -16, 0, 16, 33, 50]]
request = dict(longitude=origin[0], latitude=latitude, support_samples=samples,
               support_headings=[[[point[0] - 5 * degree, latitude], [point[0] + 5 * degree, latitude]]
                                 for point in samples], max_match_distance_m=25, max_heading_degrees=35)
aligned = dict(geometry=dict(type="LineString", coordinates=[[origin[0] - 100 * degree, latitude],
                                                            [origin[0] + 100 * degree, latitude]]))
crossing = dict(geometry=dict(type="LineString", coordinates=[[origin[0], latitude - .001],
                                                             [origin[0], latitude + .001]]))
assert module.route_support(request, aligned)["route_support_coverage"] == 1
assert module.route_support(request, crossing)["route_support_coverage"] == 0
assert module.names_match("SF Stilly R", "Stillaguamish River, South Fork")
assert not module.names_match("unnamed", "Main River")
assert not module.names_match("Tributary Creek", "Main River")
probe = dict(request, review_record_key="6:fixture", review_number=123, SiteId="fixture",
             StreamName="Main River", graph_edge_id=1, preparation_status="ready",
             max_index_distance_m=50, min_coverage=.8)

# A catchment lookup supplies only a candidate COMID: neither a wrong name nor
# a different COMID at the final projected point may trigger a basin POST.
for wrong_name, wrong_projected_id in [(True, False), (False, True)]:
    calls = []
    def fake_json(url, path, payload=None, refresh=False):
        calls.append(path.name)
        if path.name == "named_flowline.json":
            return dict(features=[dict(aligned, properties=dict(comid=1, gnis_name="Other River" if wrong_name else "Main River"))])
        return dict(features=[dict(properties=dict(comid=2 if path.name == "projected_position.json" and wrong_projected_id else 1))])
    original = module.request_json
    module.request_json = fake_json
    try:
        directory = Path.cwd() / (".nldi_test_" + uuid.uuid4().hex)
        directory.mkdir()
        try:
            result = module.process(probe, directory, recover_index=True)
            assert not result["approved"]
            assert result["status"] == ("reference_stream_name_unconfirmed" if wrong_name else "projected_comid_mismatch")
            assert "split_basin.json" not in calls
        finally:
            assert directory.resolve().is_relative_to(Path.cwd().resolve())
            shutil.rmtree(directory)
    finally:
        module.request_json = original
print("NLDI local-course and own-stream identity gates passed")
