create role rl_ct_global_ref;

grant select on ALLOWABLE_UNIT to rl_ct_global_ref;
grant select on CHEMICAL to rl_ct_global_ref;
grant select on CHEM_PRODUCT_BAK to rl_ct_global_ref;
grant select on CONSTITUENT to rl_ct_global_ref;
grant select on MANUF to rl_ct_global_ref;
grant select on PHYS_PROPERTY to rl_ct_global_ref;
grant select on STORAGE_HANDLING to rl_ct_global_ref;
grant select on STORAGE_HANDLING_VLD to rl_ct_global_ref;
grant select on TOXICOL to rl_ct_global_ref;
grant select on STORAGE_HANDLING_VU to rl_ct_global_ref;
grant select on CAS_PSTATE_TO_SUNUM to rl_ct_global_ref;
grant select on CT_REG_HAZ_MAPPING_VLD to rl_ct_global_ref;
grant select on NAME_CAS_POSITIVE to rl_ct_global_ref;
grant select on CHEMID_SEARCH to rl_ct_global_ref;
grant select on CT_REGULATION to rl_ct_global_ref;
grant select on CT_REG_HAZ_MAPPING to rl_ct_global_ref;
grant select on CHEMID_MAX_SEARCH to rl_ct_global_ref;
grant select on OTHER_IDENTIFIER to rl_ct_global_ref;

begin
	for u in (select username
			  from dba_users
			  where username like 'CT/_%' escape '/') loop
	
		execute immediate 'grant rl_ct_global_ref to ' || u.username;
	end loop;
end;

begin
	for t in (select table_name 
		      from dba_tab_privs
		      where owner = 'CT_GLOBAL'
		      and grantee = 'PUBLIC'
		      and privilege = 'SELECT') loop
		execute immediate 'revoke select on ' || t.table_name || ' from public';
	end loop;
end;