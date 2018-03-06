package com.reallifedeveloper.uu_1dl251.fitnesse;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.reallifedeveloper.uu_1dl251.domain.Patient;
import com.reallifedeveloper.uu_1dl251.domain.PatientRepository;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { "classpath:META-INF/spring-context-uu-1dl251-func-test.xml" })
public class AddPatientTest {

	@Autowired
	private PatientRepository patientRepository;

	@Test
	public void execute() {
		String patientId = "4711";
		String firstName = "Patient";
		String lastName = "Zero";
		AddPatient fixture = new AddPatient();
		fixture.setPatientId(patientId);
		fixture.setFirstName(firstName);
		fixture.setLastName(lastName);
		fixture.execute();
		Patient patient = patientRepository.findByPatientId(patientId);
		Assert.assertNotNull("Patient with ID " + patientId + " should be found", patient);
		Assert.assertEquals("Wrong patientId", patientId, patient.patientId());
		Assert.assertEquals("Wrong firstName", firstName, patient.firstName());
		Assert.assertEquals("Wrong lastName", lastName, patient.lastName());
		Assert.assertNotNull("Patient record should not be null", patient.record());
	}
}
