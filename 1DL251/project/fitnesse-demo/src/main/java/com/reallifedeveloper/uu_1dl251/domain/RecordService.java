package com.reallifedeveloper.uu_1dl251.domain;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class RecordService {

	@Autowired
	private PatientRepository patientRepository;

	public void addRecordEntry(User user, Patient patient, String entry) {
		if (!user.hasRole("DOCTOR")) {
			throw new IllegalArgumentException("Only doctors can add records");
		}
		RecordEntry recordEntry = new RecordEntry(patient.record(), entry);
		patient.record().addEntry(recordEntry);
		patientRepository.save(patient);
	}

	public Record findRecordForPatient(String patientId) {
		Patient patient = patientRepository.findByPatientId(patientId);
		if (patient == null) {
			throw new IllegalArgumentException("Patient with ID " + patientId + " not found");
		}
		return patient.record();
	}
}
