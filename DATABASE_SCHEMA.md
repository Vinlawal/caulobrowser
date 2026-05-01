# Database Schema

The app reads from a DuckDB database with five tables:

- `genes` — `gene_id` (VARCHAR PK, CCNA_XXXXX NA1000 locus tag), `cc_tag` (CC_XXXX CB15 legacy tag), `gene_name`, `ncbi_protein_id`, `gene_biotype`, `description`

- `experiments` — `experiment_id` (VARCHAR PK), `display_label`, `experiment_class`, `data_type`, `strain`, `genetic_background`, `treatment`, `treatment_level`, `growth_phase`, `media`, `ref_strain`, `ref_treatment`, `ref_treatment_level`, `ref_growth_phase`, `ref_media`, `lab_group`, `doi`, `geo_id`, `date_added`

- `experiment_conditions` — (`experiment_id`, `condition_label`) composite PK; `condition_order` (INTEGER), `condition_value` (DOUBLE), `condition_units`, `display_label`. FK → `experiments`

- `de_results` — (`gene_id`, `experiment_id`) composite PK; `log2fc` (DOUBLE NOT NULL), `padj` (DOUBLE). FK → `genes` + `experiments`

- `timecourse_expression` — (`gene_id`, `experiment_id`, `condition_label`) composite PK; `expression_value` (DOUBLE NOT NULL). FK → `genes`, `experiments`, and `experiment_conditions(experiment_id, condition_label)`

Indexes on: `de_results(experiment_id)`, `de_results(gene_id)`, `timecourse_expression(gene_id)`, `timecourse_expression(experiment_id)`, `timecourse_expression(gene_id, experiment_id)`, `experiments(experiment_class)`, `experiments(data_type)`, `experiments(lab_group)`
