package com.reallifedeveloper.uu_1dl251.domain;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "patient")
public class Patient {

	@Id
	@Column(name = "patient_id")
	private String patientId;

	@Column(name = "first_name")
	private String firstName;

	@Column(name = "last_name")
	private String lastName;

	@OneToOne(cascade = CascadeType.ALL, fetch = FetchType.EAGER, mappedBy = "patient")
	private Record record;

	public Patient(String patientId, String firstName, String lastName) {
		this.patientId = patientId;
		this.firstName = firstName;
		this.lastName = lastName;
		this.record = new Record(this);
	}

	// Required by Hibernate
	public Patient() {
	}

	public String patientId() {
		return patientId;
	}

	public String firstName() {
		return firstName;
	}

	public String lastName() {
		return lastName;
	}

	public Record record() {
		return record;
	}

	@Override
	public String toString() {
		return "Patient{patientId=" + patientId + ", firstName=" + firstName + ", lastName=" + lastName + "}";
	}

}
