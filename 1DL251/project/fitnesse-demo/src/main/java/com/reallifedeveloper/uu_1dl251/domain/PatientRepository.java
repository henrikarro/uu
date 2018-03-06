package com.reallifedeveloper.uu_1dl251.domain;

public interface PatientRepository {

	Patient findByPatientId(String patientId);

	<P extends Patient> P save(P patient);

}
