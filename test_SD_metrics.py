from sdmetrics import load_demo
from sdmetrics.reports.single_table import QualityReport

real_data, synthetic_data, metadata = load_demo(modality='single_table')

my_report = QualityReport()
my_report.generate(real_data, synthetic_data, metadata)

my_report.save(filepath='Output/demo_data_quality_report.pkl')

my_report = QualityReport.load(filepath='Output/demo_data_quality_report.pkl')

from sdmetrics.single_column import BoundaryAdherence

BoundaryAdherence.compute(
    real_data['start_date'],
    synthetic_data['start_date']
)

from sdmetrics.single_table import NewRowSynthesis

NewRowSynthesis.compute(
    real_data,
    synthetic_data,
    metadata
)





