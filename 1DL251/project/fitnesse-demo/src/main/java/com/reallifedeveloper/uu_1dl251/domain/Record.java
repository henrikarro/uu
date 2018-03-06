package com.reallifedeveloper.uu_1dl251.domain;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.MapsId;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "record")
public class Record {

	@Id
	@Column(name = "record_id")
	private String id;

	@OneToMany(mappedBy = "record", cascade = CascadeType.ALL, fetch = FetchType.EAGER)
	private List<RecordEntry> entries = new ArrayList<>();

	@OneToOne
	@MapsId
	@JoinColumn(name = "record_id")
	private Patient patient;

	public Record(Patient patient) {
		this.id = patient.patientId();
		this.patient = patient;
	}

	// Required by Hibernate
	Record() {
	}

	public List<RecordEntry> entries() {
		return entries;
	}

	public void addEntry(RecordEntry recordEntry) {
		entries.add(recordEntry);
	}

}
