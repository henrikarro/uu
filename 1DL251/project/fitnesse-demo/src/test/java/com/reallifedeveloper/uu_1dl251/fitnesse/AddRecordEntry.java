package com.reallifedeveloper.uu_1dl251.fitnesse;

import com.reallifedeveloper.tools.test.fitnesse.AbstractFitNesseFixture;
import com.reallifedeveloper.uu_1dl251.domain.Patient;
import com.reallifedeveloper.uu_1dl251.domain.PatientRepository;
import com.reallifedeveloper.uu_1dl251.domain.RecordService;
import com.reallifedeveloper.uu_1dl251.domain.User;
import com.reallifedeveloper.uu_1dl251.domain.UserRepository;

public class AddRecordEntry extends AbstractFitNesseFixture {

	private User user;
	private Patient patient;
	private String recordEntry;
	private String errorMessage = "";

	private UserRepository userRepository;
	private PatientRepository patientRepository;
	private RecordService recordService;

	public AddRecordEntry() {
		super("META-INF/spring-context-uu-1dl251-func-test.xml");
		this.userRepository = getBean(UserRepository.class);
		this.patientRepository = getBean(PatientRepository.class);
		this.recordService = getBean(RecordService.class);
	}

	public void setUserId(String userId) {
		this.user = userRepository.findByUserId(userId);
	}

	public void setPatientId(String patientId) {
		this.patient = patientRepository.findByPatientId(patientId);
	}

	public void setEntry(String recordEntry) {
		this.recordEntry = recordEntry;
	}

	public void execute() {
		try {
			recordService.addRecordEntry(user, patient, recordEntry);
		} catch (IllegalArgumentException e) {
			this.errorMessage = e.getMessage();
		}
	}

	public void reset() {
		this.errorMessage = "";
	}

	public String errorMessage() {
		return errorMessage;
	}
}
