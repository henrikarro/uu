package com.reallifedeveloper.uu_1dl251.infrastructure.persistence;

import org.springframework.data.jpa.repository.JpaRepository;

import com.reallifedeveloper.uu_1dl251.domain.Patient;
import com.reallifedeveloper.uu_1dl251.domain.PatientRepository;

public interface JpaPatientRepository extends PatientRepository, JpaRepository<Patient, String> {

}
