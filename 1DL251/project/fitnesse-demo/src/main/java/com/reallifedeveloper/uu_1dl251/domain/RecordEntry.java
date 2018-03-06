package com.reallifedeveloper.uu_1dl251.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "record_entry")
public class RecordEntry {

	@Id
	@Column(name = "record_entry_id")
	@GeneratedValue(strategy = GenerationType.AUTO)
	private Long id;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "record_id")
	private Record record;

	@Column(name = "entry")
	private String entry;

	public RecordEntry(Long id, Record record, String entry) {
		this.id = id;
		this.record = record;
		this.entry = entry;
	}

	public RecordEntry(Record record, String entry) {
		this(null, record, entry);
	}

	// Required by Hibernate
	RecordEntry() {
	}

	public Record record() {
		return record;
	}

	public String entry() {
		return entry;
	}
}
